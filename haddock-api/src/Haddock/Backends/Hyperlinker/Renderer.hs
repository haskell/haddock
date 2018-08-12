{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Haddock.Backends.Hyperlinker.Renderer (render) where


import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Utils

import qualified GHC
import qualified Name as GHC
import qualified Unique as GHC
import HieTypes hiding (Span)
import HieUtils
import DynFlags (DynFlags)

import System.FilePath.Posix ((</>))

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (guard)

import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html

import FastString
import SrcLoc

type StyleClass = String


render :: Maybe FilePath -> Maybe FilePath -> DynFlags -> SrcMap -> HieAST HieTypeFix -> [Token]
       -> Html
render mcss mjs df srcs ast tokens = header mcss mjs <> body df srcs ast tokens

body :: DynFlags -> SrcMap -> HieAST HieTypeFix -> [Token] -> Html
body df srcs ast tokens = Html.body . Html.pre $ hypsrc
  where
    hypsrc = renderWithAst df srcs ast tokens

header :: Maybe FilePath -> Maybe FilePath -> Html
header mcss mjs
    | isNothing mcss && isNothing mjs = Html.noHtml
header mcss mjs =
    Html.header $ css mcss <> js mjs
  where
    css Nothing = Html.noHtml
    css (Just cssFile) = Html.thelink Html.noHtml !
        [ Html.rel "stylesheet"
        , Html.thetype "text/css"
        , Html.href cssFile
        ]
    js Nothing = Html.noHtml
    js (Just scriptFile) = Html.script Html.noHtml !
        [ Html.thetype "text/javascript"
        , Html.src scriptFile
        ]

splitTokens :: HieAST HieTypeFix -> [Token] -> ([Token],[Token],[Token])
splitTokens ast toks' = (initial++before,during,after)
  where
    (initial,toks) = span ((== fsLit "lexing") . srcSpanFile . tkSpan) toks'
    (before,rest) = span leftOf toks
    (during,after) = span inAst rest
    leftOf t = realSrcSpanEnd (tkSpan t) <= realSrcSpanStart nodeSp
    inAst t = nodeSp `containsSpan` tkSpan t
    nodeSp = nodeSpan ast

renderWithAst :: DynFlags -> SrcMap -> HieAST HieTypeFix -> [Token] -> Html
renderWithAst df srcs ast toks = anchored $ case toks of
    [tok] | nodeSpan ast == tkSpan tok -> richToken df srcs (nodeInfo ast) tok
    xs -> go (nodeChildren ast) xs
  where
    go _ [] = mempty
    go [] xs = foldMap renderToken xs
    go (cur:rest) xs =
        foldMap renderToken before <> renderWithAst df srcs cur during <> go rest after
      where
        (before,during,after) = splitTokens cur xs
    anchored c = Map.foldrWithKey anchorOne c (nodeIdentifiers $ nodeInfo ast)
    anchorOne n dets c = externalAnchor n d $ internalAnchor n d c
      where d = identInfo dets

renderToken :: Token -> Html
renderToken Token{..}
  | tkType == TkSpace = renderSpace (GHC.srcSpanStartLine tkSpan) tkValue
  | otherwise = tokenSpan ! [ multiclass style ]
      where
        style = tokenStyle tkType
        tokenSpan = Html.thespan (Html.toHtml tkValue)

-- | Given information about the source position of definitions, render a token
richToken :: DynFlags -> SrcMap -> NodeInfo HieTypeFix -> Token -> Html
richToken df srcs details Token{..}
    | tkType == TkSpace = renderSpace (GHC.srcSpanStartLine tkSpan) tkValue
    | otherwise = annotate df details $ linked content
  where
    content = tokenSpan ! [ multiclass style ]
    tokenSpan = Html.thespan (Html.toHtml tkValue)
    style = tokenStyle tkType ++ concatMap richTokenStyle contexts

    contexts = concatMap (Set.elems . identInfo) . Map.elems . nodeIdentifiers $ details

    -- pick an arbitary identifier to hyperlink with
    identDet = Map.lookupMin . nodeIdentifiers $ details

    -- If we have name information, we can make links
    linked = case identDet of
      Just (n,_) -> hyperlink srcs n
      Nothing -> id

annotate :: DynFlags -> NodeInfo HieTypeFix -> Html -> Html
annotate df ni content =
    Html.thespan (annot <> content) ! [ Html.theclass "annot" ]
  where
    annot
      | not (null annotation) =
          Html.thespan (Html.toHtml annotation) ! [ Html.theclass "annottext" ]
      | otherwise = mempty
    annotation = typ ++ identTyps
    typ = unlines $ map (renderHieType df) $ nodeType ni
    typedIdents = [ (n,t) | (n, identType -> Just t) <- Map.toList $ nodeIdentifiers ni ]
    identTyps
      | length typedIdents > 1 || null typ
          = concatMap (\(n,t) -> printName n ++ " :: " ++ renderHieType df t ++ "\n") typedIdents
      | otherwise = ""

    printName (Right n) = unpackFS $ GHC.occNameFS $ GHC.getOccName n
    printName (Left n) = GHC.moduleNameString n


richTokenStyle :: ContextInfo -> [StyleClass]
richTokenStyle Use = ["hs-var"]
richTokenStyle IEThing{} = ["hs-var"]
richTokenStyle TyDecl = ["hs-var"]
richTokenStyle ValBind{} = ["hs-var"]
richTokenStyle PatternBind{} = ["hs-var"]
richTokenStyle ClassTyDecl{} = ["hs-var"]
richTokenStyle RecField{} = ["hs-var"]
richTokenStyle Decl{} = ["hs-type"]
richTokenStyle TyVarBind{} = ["hs-type"]
richTokenStyle _ = []

tokenStyle :: TokenType -> [StyleClass]
tokenStyle TkIdentifier = ["hs-identifier"]
tokenStyle TkKeyword = ["hs-keyword"]
tokenStyle TkString = ["hs-string"]
tokenStyle TkChar = ["hs-char"]
tokenStyle TkNumber = ["hs-number"]
tokenStyle TkOperator = ["hs-operator"]
tokenStyle TkGlyph = ["hs-glyph"]
tokenStyle TkSpecial = ["hs-special"]
tokenStyle TkSpace = []
tokenStyle TkComment = ["hs-comment"]
tokenStyle TkCpp = ["hs-cpp"]
tokenStyle TkPragma = ["hs-pragma"]
tokenStyle TkUnknown = []

multiclass :: [StyleClass] -> HtmlAttr
multiclass = Html.theclass . intercalate " "

externalAnchor :: Identifier -> Set.Set ContextInfo -> Html -> Html
externalAnchor (Right name) contexts content
    |  not (GHC.isInternalName name)
    && any isBinding contexts =
        Html.thespan content ! [ Html.identifier $ externalAnchorIdent name ]
externalAnchor _ _ content = content

isBinding :: ContextInfo -> Bool
isBinding (ValBind RegularBind _ _) = True
isBinding PatternBind{} = True
isBinding Decl{} = True
isBinding (RecField RecFieldDecl _) = True
isBinding TyVarBind{} = True
isBinding ClassTyDecl{} = True
isBinding _ = False

internalAnchor :: Identifier -> Set.Set ContextInfo -> Html -> Html
internalAnchor (Right name) contexts content
  | GHC.isInternalName name && any isBinding contexts =
      Html.thespan content ! [ Html.identifier $ internalAnchorIdent name ]
internalAnchor _ _ content = content

externalAnchorIdent :: GHC.Name -> String
externalAnchorIdent = hypSrcNameUrl

internalAnchorIdent :: GHC.Name -> String
internalAnchorIdent = ("local-" ++) . show . GHC.getKey . GHC.nameUnique

hyperlink :: SrcMap -> Identifier -> Html -> Html
hyperlink srcs ident = case ident of
    Right name ->
        if GHC.isInternalName name
        then internalHyperlink name
        else externalNameHyperlink srcs name
    Left name -> externalModHyperlink srcs name

internalHyperlink :: GHC.Name -> Html -> Html
internalHyperlink name content =
    Html.anchor content ! [ Html.href $ "#" ++ internalAnchorIdent name ]

externalNameHyperlink :: SrcMap -> GHC.Name -> Html -> Html
externalNameHyperlink srcs name content = case Map.lookup mdl srcs of
    Just SrcLocal -> Html.anchor content !
        [ Html.href $ hypSrcModuleNameUrl mdl name ]
    Just (SrcExternal path) -> Html.anchor content !
        [ Html.href $ path </> hypSrcModuleNameUrl mdl name ]
    Nothing -> content
  where
    mdl = GHC.nameModule name

externalModHyperlink :: SrcMap -> GHC.ModuleName -> Html -> Html
externalModHyperlink srcs name content =
    let srcs' = Map.mapKeys GHC.moduleName srcs in
    case Map.lookup name srcs' of
      Just SrcLocal -> Html.anchor content !
        [ Html.href $ hypSrcModuleUrl' name ]
      Just (SrcExternal path) -> Html.anchor content !
        [ Html.href $ path </> hypSrcModuleUrl' name ]
      Nothing -> content


renderSpace :: Int -> String -> Html
renderSpace _ [] = Html.noHtml
renderSpace line ('\n':rest) = mconcat
    [ Html.thespan . Html.toHtml $ "\n"
    , lineAnchor (line + 1)
    , renderSpace (line + 1) rest
    ]
renderSpace line space =
    let (hspace, rest) = span (/= '\n') space
    in (Html.thespan . Html.toHtml) hspace <> renderSpace line rest


lineAnchor :: Int -> Html
lineAnchor line = Html.thespan Html.noHtml ! [ Html.identifier $ hypSrcLineUrl line ]
