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
import HieDebug

import System.FilePath.Posix ((</>))

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (guard)

import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html

import FastString

type StyleClass = String


render :: Maybe FilePath -> Maybe FilePath -> SrcMap -> HieAST HieTypeFix -> [Token]
       -> Html
render mcss mjs srcs ast tokens = header mcss mjs <> body srcs ast tokens

body :: SrcMap -> HieAST HieTypeFix -> [Token] -> Html
body srcs ast tokens = Html.body . Html.pre $ hypsrc
  where
    hypsrc = mconcat . map (richToken srcs ast) $ tokens


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

-- | Given information about the source position of definitions, render a token
richToken :: SrcMap -> HieAST HieTypeFix -> Token -> Html
richToken srcs ast Token{..}
    | tkType == TkSpace = renderSpace (GHC.srcSpanStartLine tkSpan) tkValue
    | otherwise = annotated $ linked content
  where
    content = tokenSpan ! [ multiclass style ]
    tokenSpan = Html.thespan (Html.toHtml tkValue)
    style = tokenStyle tkType ++ maybe [] (concatMap richTokenStyle) contexts

    details = do
      ast <- selectSmallestContaining tkSpan ast
      return $ nodeInfo ast

    contexts = concatMap (Set.elems . identInfo) . Map.elems . nodeIdentifiers <$> details

    identDet = details >>=
      Map.lookupMin . fmap identInfo . nodeIdentifiers

    -- If we have name information, we can make links
    linked = case identDet of
      Just (n,d) -> externalAnchor n d . internalAnchor n d . hyperlink srcs n
      Nothing -> id

    annotated = case details of
      Just d -> annotate d
      Nothing -> id

annotate :: NodeInfo HieTypeFix -> Html -> Html
annotate ni content =
    Html.thespan (annot <> content) ! [ Html.theclass "annot" ]
  where
    annot
      | not (null annotation) =
          Html.thespan (Html.toHtml annotation) ! [ Html.theclass "annottext" ]
      | otherwise = mempty
    annotation = typ ++ identTyps
    typ = unlines $ map show $ nodeType ni
    typedIdents = [ (n,t) | (n, identType -> Just t) <- Map.toList $ nodeIdentifiers ni ]
    identTyps
      | length typedIdents > 1 || null typ
          = concatMap (\(n,t) -> printName n ++ " :: " ++ show t ++ "\n") typedIdents
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
