{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Haddock.Backends.Hyperlinker.Renderer (render) where


import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Utils

import DynFlags ( DynFlags )
import FastString (fsLit)
import HieTypes
import HieUtils ( renderHieType )
import Module   ( ModuleName, moduleName, moduleNameString )
import Name     ( getOccString, isInternalName, Name, nameModule, nameUnique )
import SrcLoc
import Unique   ( getKey )
import Encoding ( utf8DecodeByteString )

import System.FilePath.Posix ((</>))

import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html


type StyleClass = String

-- | Produce the HTML corresponding to a hyperlinked Haskell source
render
  :: Maybe FilePath    -- ^ path to the CSS file
  -> Maybe FilePath    -- ^ path to the JS file
  -> DynFlags          -- ^ used to render types
  -> SrcMap            -- ^ Paths to sources
  -> HieAST HieTypeFix -- ^ ASTs from @.hie@ files
  -> [Token]           -- ^ tokens to render
  -> Html
render mcss mjs df srcs ast tokens = header mcss mjs <> body df srcs ast tokens

body :: DynFlags -> SrcMap -> HieAST HieTypeFix -> [Token] -> Html
body df srcs ast tokens = Html.body . Html.pre $ hypsrc
  where
    hypsrc = renderWithAst df srcs ast tokens

header :: Maybe FilePath -> Maybe FilePath -> Html
header Nothing Nothing = Html.noHtml
header mcss mjs = Html.header $ css mcss <> js mjs
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

-- | Turn a list of tokens into hyperlinked sources, threading in relevant link
-- information from the 'HieAST'.
renderWithAst :: DynFlags -> SrcMap -> HieAST HieTypeFix -> [Token] -> Html
renderWithAst df srcs ast toks = anchored $ case toks of
    [tok] | nodeSpan ast == tkSpan tok -> richToken df srcs (nodeInfo ast) tok

    -- NB: the GHC lexer lexes backquoted identifiers and parenthesized operators
    -- as multiple tokens.
    --
    --  * @a `elem` b@ turns into @[a, `, elem, `, b]@ (excluding space tokens)
    --  * @(+) 1 2@    turns into @[(, +, ), 1, 2]@    (excluding space tokens)
    --
    -- However, the HIE ast considers @`elem`@ and @(+)@ to be single nodes. In
    -- order to make sure these get hyperlinked properly, we intercept these
    -- special sequences of tokens and turn merge then into just one identifier
    -- or operator token.
    [BacktickTok s1,  tok @ Token{ tkType = TkIdentifier }, BacktickTok s2]
          | realSrcSpanStart s1 == realSrcSpanStart (nodeSpan ast)
          , realSrcSpanEnd s2   == realSrcSpanEnd (nodeSpan ast)
          -> richToken df srcs (nodeInfo ast)
                       (Token{ tkValue = "`" <> tkValue tok <> "`"
                             , tkType = TkOperator
                             , tkSpan = nodeSpan ast })
    [OpenParenTok s1, tok @ Token{ tkType = TkOperator }, CloseParenTok s2]
          | realSrcSpanStart s1 == realSrcSpanStart (nodeSpan ast)
          , realSrcSpanEnd s2   == realSrcSpanEnd (nodeSpan ast)
          -> richToken df srcs (nodeInfo ast)
                       (Token{ tkValue = "(" <> tkValue tok <> ")"
                             , tkType = TkOperator
                             , tkSpan = nodeSpan ast })

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
    | tkType == TkSpace = renderSpace (srcSpanStartLine tkSpan) tkValue'
    | otherwise = tokenSpan ! [ multiclass style ]
  where
    tkValue' = filterCRLF $ utf8DecodeByteString tkValue
    style = tokenStyle tkType
    tokenSpan = Html.thespan (Html.toHtml tkValue')

-- | Given information about the source position of definitions, render a token
richToken :: DynFlags -> SrcMap -> NodeInfo HieTypeFix -> Token -> Html
richToken df srcs details Token{..}
    | tkType == TkSpace = renderSpace (srcSpanStartLine tkSpan) tkValue'
    | otherwise = annotate df details $ linked content
  where
    tkValue' = filterCRLF $ utf8DecodeByteString tkValue
    content = tokenSpan ! [ multiclass style ]
    tokenSpan = Html.thespan (Html.toHtml tkValue')
    style = tokenStyle tkType ++ concatMap richTokenStyle contexts

    contexts = concatMap (Set.elems . identInfo) . Map.elems . nodeIdentifiers $ details

    -- pick an arbitary identifier to hyperlink with
    identDet = Map.lookupMin . nodeIdentifiers $ details

    -- If we have name information, we can make links
    linked = case identDet of
      Just (n,_) -> hyperlink srcs n
      Nothing -> id

-- | Remove CRLFs from source
filterCRLF :: String -> String
filterCRLF ('\r':'\n':cs) = '\n' : filterCRLF cs
filterCRLF (c:cs) = c : filterCRLF cs
filterCRLF [] = []

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

    printName :: Either ModuleName Name -> String
    printName = either moduleNameString getOccString


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
multiclass = Html.theclass . unwords

externalAnchor :: Identifier -> Set.Set ContextInfo -> Html -> Html
externalAnchor (Right name) contexts content
  | not (isInternalName name)
  , any isBinding contexts
  = Html.thespan content ! [ Html.identifier $ externalAnchorIdent name ]
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
  | isInternalName name
  , any isBinding contexts
  = Html.thespan content ! [ Html.identifier $ internalAnchorIdent name ]
internalAnchor _ _ content = content

externalAnchorIdent :: Name -> String
externalAnchorIdent = hypSrcNameUrl

internalAnchorIdent :: Name -> String
internalAnchorIdent = ("local-" ++) . show . getKey . nameUnique

hyperlink :: SrcMap -> Identifier -> Html -> Html
hyperlink srcs ident = case ident of
    Right name | isInternalName name -> internalHyperlink name
               | otherwise -> externalNameHyperlink srcs name
    Left name -> externalModHyperlink srcs name

internalHyperlink :: Name -> Html -> Html
internalHyperlink name content =
    Html.anchor content ! [ Html.href $ "#" ++ internalAnchorIdent name ]

externalNameHyperlink :: SrcMap -> Name -> Html -> Html
externalNameHyperlink srcs name content = case Map.lookup mdl srcs of
    Just SrcLocal -> Html.anchor content !
        [ Html.href $ hypSrcModuleNameUrl mdl name ]
    Just (SrcExternal path) -> Html.anchor content !
        [ Html.href $ spliceURL Nothing (Just mdl) (Just name) Nothing (".." </> path) ]
    Nothing -> content
  where
    mdl = nameModule name

externalModHyperlink :: SrcMap -> ModuleName -> Html -> Html
externalModHyperlink srcs name content =
    let srcs' = Map.mapKeys moduleName srcs in
    case Map.lookup name srcs' of
      Just SrcLocal -> Html.anchor content !
        [ Html.href $ hypSrcModuleUrl' name ]
      Just (SrcExternal path) -> Html.anchor content !
        [ Html.href $ path </> hypSrcModuleUrl' name ]
      Nothing -> content


renderSpace :: Int -> String -> Html
renderSpace _ [] = Html.noHtml
renderSpace line ('\n':rest) = mconcat
    [ Html.thespan (Html.toHtml '\n')
    , lineAnchor (line + 1)
    , renderSpace (line + 1) rest
    ]
renderSpace line space =
    let (hspace, rest) = span (/= '\n') space
    in (Html.thespan . Html.toHtml) hspace <> renderSpace line rest


lineAnchor :: Int -> Html
lineAnchor line = Html.thespan Html.noHtml ! [ Html.identifier $ hypSrcLineUrl line ]
