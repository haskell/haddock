{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Haddock.Backends.Hyperlinker.Renderer (render) where


import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Utils

import qualified Data.ByteString as BS

import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils (RefMap, EvidenceInfo(..), findEvidenceUse, getEvidenceTree, emptyNodeInfo, isEvidenceContext)
import GHC.Unit.Module ( ModuleName, moduleNameString )
import GHC.Types.Name   ( getOccString, isInternalName, Name, nameModule, nameUnique, nameSrcSpan )
import GHC.Types.SrcLoc
import GHC.Types.Unique ( getKey )
import GHC.Utils.Encoding ( utf8DecodeByteString )

import System.FilePath.Posix ((</>))

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable
import Data.Tree (flatten)

import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html


type StyleClass = String

-- | Produce the HTML corresponding to a hyperlinked Haskell source
render
  :: Maybe FilePath    -- ^ path to the CSS file
  -> Maybe FilePath    -- ^ path to the JS file
  -> SrcMaps            -- ^ Paths to sources
  -> HieAST PrintedType  -- ^ ASTs from @.hie@ files
  -> RefMap PrintedType
  -> [Token]       -- ^ tokens to render
  -> Html
render mcss mjs srcs ast refmap tokens = header mcss mjs <> body srcs ast refmap tokens

body :: SrcMaps -> HieAST PrintedType -> RefMap PrintedType -> [Token] -> Html
body srcs ast refmap tokens = Html.body . Html.pre $ hypsrc
  where
    hypsrc = renderWithAst srcs ast refmap tokens

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


splitTokens :: HieAST PrintedType -> [Token] -> ([Token],[Token],[Token])
splitTokens ast toks = (before,during,after)
  where
    (before,rest) = span leftOf toks
    (during,after) = span inAst rest
    leftOf t = realSrcSpanEnd (tkSpan t) <= realSrcSpanStart nodeSp
    inAst t = nodeSp `containsSpan` tkSpan t
    nodeSp = nodeSpan ast

-- | Turn a list of tokens into hyperlinked sources, threading in relevant link
-- information from the 'HieAST'.
renderWithAst :: SrcMaps -> HieAST PrintedType -> RefMap PrintedType -> [Token] -> Html
renderWithAst srcs Node{..} refmap toks = anchored $ case toks of

    [tok] | nodeSpan == tkSpan tok -> richToken srcs nodeInfo refmap tok

    -- NB: the GHC lexer lexes backquoted identifiers and parenthesized operators
    -- as multiple tokens.
    --
    --  * @a `elem` b@ turns into @[a, `, elem, `, b]@ (excluding space tokens)
    --  * @(+) 1 2@    turns into @[(, +, ), 1, 2]@    (excluding space tokens)
    --
    -- However, the HIE ast considers @`elem`@ and @(+)@ to be single nodes. In
    -- order to make sure these get hyperlinked properly, we intercept these
    -- special sequences of tokens and merge them into just one identifier or
    -- operator token.
    [BacktickTok s1, tok@Token{ tkType = TkIdentifier }, BacktickTok s2]
          | realSrcSpanStart s1 == realSrcSpanStart nodeSpan
          , realSrcSpanEnd s2   == realSrcSpanEnd nodeSpan
          -> richToken srcs nodeInfo refmap
                       (Token{ tkValue = "`" <> tkValue tok <> "`"
                             , tkType = TkOperator
                             , tkSpan = nodeSpan })
    [OpenParenTok s1, tok@Token{ tkType = TkOperator }, CloseParenTok s2]
          | realSrcSpanStart s1 == realSrcSpanStart nodeSpan
          , realSrcSpanEnd s2   == realSrcSpanEnd nodeSpan
          -> richToken srcs nodeInfo refmap
                       (Token{ tkValue = "(" <> tkValue tok <> ")"
                             , tkType = TkOperator
                             , tkSpan = nodeSpan })

    _ -> go nodeChildren toks
  where
    nodeInfo = maybe emptyNodeInfo id (Map.lookup SourceInfo $ getSourcedNodeInfo sourcedNodeInfo)
    go _ [] = mempty
    go [] xs = foldMap renderToken xs
    go (cur:rest) xs =
        foldMap renderToken before <> renderWithAst srcs cur refmap during <> go rest after
      where
        (before,during,after) = splitTokens cur xs
    anchored c = Map.foldrWithKey anchorOne c (nodeIdentifiers nodeInfo)
    anchorOne n dets c = externalAnchor n d $ internalAnchor n d c
      where d = identInfo dets

renderToken :: Token -> Html
renderToken Token{..}
    | BS.null tkValue = mempty
    | tkType == TkSpace = renderSpace (srcSpanStartLine tkSpan) tkValue'
    | otherwise = tokenSpan ! [ multiclass style ]
  where
    tkValue' = filterCRLF $ utf8DecodeByteString tkValue
    style = tokenStyle tkType
    tokenSpan = Html.thespan (Html.toHtml tkValue')


-- | Given information about the source position of definitions, render a token
richToken :: SrcMaps -> NodeInfo PrintedType -> RefMap PrintedType -> Token -> Html
richToken srcs details refmap Token{..}
    | tkType == TkSpace = renderSpace (srcSpanStartLine tkSpan) tkValue'
    | otherwise = annotate srcs details refmap $ linked content
  where
    tkValue' = filterCRLF $ utf8DecodeByteString tkValue
    content = tokenSpan ! [ multiclass style ]
    tokenSpan = Html.thespan (Html.toHtml tkValue')
    style = tokenStyle tkType ++ concatMap (richTokenStyle (null (nodeType details))) contexts

    contexts = concatMap (Set.elems . identInfo) . Map.elems . nodeIdentifiers $ details

    -- pick an arbitary non-evidence identifier to hyperlink with
    identDet = Map.lookupMin $ Map.filter notEvidence $ nodeIdentifiers $ details
    notEvidence = not . any isEvidenceContext . identInfo

    -- If we have name information, we can make links
    linked = case identDet of
      Just (n,_) -> hyperlink srcs n
      Nothing -> id

-- | Remove CRLFs from source
filterCRLF :: String -> String
filterCRLF ('\r':'\n':cs) = '\n' : filterCRLF cs
filterCRLF (c:cs) = c : filterCRLF cs
filterCRLF [] = []

annotate :: SrcMaps -> NodeInfo PrintedType -> RefMap PrintedType -> Html -> Html
annotate srcs ni refmap content =
    Html.thespan (annot <> content) ! [ Html.theclass "annot" ]
  where
    annot
      | not (null annotation) =
          Html.thespan (Html.toHtml annotation <> fold evs) ! [ Html.theclass "annottext" ]
      | otherwise = mempty
    annotation = typ ++ identTyps
    typ = unlines (nodeType ni)
    typedIdents = [ (n,t) | (n, c@(identType -> Just t)) <- Map.toList $ nodeIdentifiers ni
                          , not (any isEvidenceContext $ identInfo c) ]
    identTyps
      | length typedIdents > 1 || null (nodeType ni)
          = concatMap (\(n,t) -> printName n ++ " :: " ++ t ++ "\n") typedIdents
      | otherwise = ""

    evidenceVars = findEvidenceUse $ nodeIdentifiers ni
    evTrees = mapMaybe (getEvidenceTree refmap) evidenceVars
    evs = mapMaybe (renderEvidence srcs) $ concatMap flatten evTrees

    printName :: Either ModuleName Name -> String
    printName = either moduleNameString getOccString

renderEvidence :: SrcMaps -> EvidenceInfo PrintedType -> Maybe Html
renderEvidence srcs EvidenceInfo{..} =
  case varDesc of
    Nothing -> Nothing
    Just d -> Just $ d <> (Html.toHtml $ " of the constraint type " <> evidenceType <> "\n")
  where
    linkInst :: String -> Maybe Html
    linkInst c = case nameSrcSpan evidenceVar of
      _ -> Just $ hyperlink srcs (Right evidenceVar) (Html.toHtml c)

    linkOther :: Maybe RealSrcSpan -> String -> Maybe Html
    linkOther Nothing c = linkInst c
    linkOther (Just spn) c =
        Just $ Html.anchor (Html.toHtml c) ! [ Html.href $ "#" ++ hypSrcLineUrl (srcLocLine $ realSrcSpanStart spn)]
    varDesc = case evidenceDetails of
      Just (src,_,spn) -> case src of
        EvPatternBind -> linkOther spn "Evidence bound by a pattern"
        EvSigBind -> linkOther spn "Evidence bound by a type signature"
        EvWrapperBind -> linkOther spn "Evidence bound by a HsWrapper"
        EvImplicitBind -> linkOther spn ("Implicit variable " ++ getOccString evidenceVar)
        EvInstBind False cls -> linkInst $ "Instance of class: " ++ getOccString cls
        EvInstBind True cls -> linkInst $ "Evidence bound by a superclass of: " ++ getOccString cls
        EvLetBind{} -> Nothing -- linkOther spn "Evidence bound by a let"
      Nothing -> linkInst "External instance"

richTokenStyle
  :: Bool         -- ^ are we lacking a type annotation?
  -> ContextInfo  -- ^ in what context did this token show up?
  -> [StyleClass]
richTokenStyle True  Use               = ["hs-type"]
richTokenStyle False Use               = ["hs-var"]
richTokenStyle  _    RecField{}        = ["hs-var"]
richTokenStyle  _    PatternBind{}     = ["hs-var"]
richTokenStyle  _    MatchBind{}       = ["hs-var"]
richTokenStyle  _    TyVarBind{}       = ["hs-type"]
richTokenStyle  _    ValBind{}         = ["hs-var"]
richTokenStyle  _    TyDecl            = ["hs-type"]
richTokenStyle  _    ClassTyDecl{}     = ["hs-type"]
richTokenStyle  _    Decl{}            = ["hs-var"]
richTokenStyle  _    IEThing{}         = []  -- could be either a value or type
richTokenStyle  _    EvidenceVarBind{} = []
richTokenStyle  _    EvidenceVarUse{}  = []

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
isBinding (EvidenceVarBind EvLetBind{} _ _) = False -- Don't want to add useless anchors for let binds
isBinding EvidenceVarBind{} = True
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

-- | Generate the HTML hyperlink for an identifier
hyperlink :: SrcMaps -> Identifier -> Html -> Html
hyperlink (srcs, srcs') ident = case ident of
    Right name | isInternalName name -> internalHyperlink name
               | otherwise -> externalNameHyperlink name
    Left name -> externalModHyperlink name

  where
    internalHyperlink name content =
        Html.anchor content ! [ Html.href $ "#" ++ internalAnchorIdent name ]

    externalNameHyperlink name content = case Map.lookup mdl srcs of
        Just SrcLocal -> Html.anchor content !
            [ Html.href $ hypSrcModuleNameUrl mdl name ]
        Just (SrcExternal path) -> Html.anchor content !
            [ Html.href $ spliceURL Nothing (Just mdl) (Just name) Nothing (".." </> path) ]
        Nothing -> content
      where
        mdl = nameModule name

    externalModHyperlink moduleName content =
        case Map.lookup moduleName srcs' of
          Just SrcLocal -> Html.anchor content !
            [ Html.href $ hypSrcModuleUrl' moduleName ]
          Just (SrcExternal path) -> Html.anchor content !
            [ Html.href $ spliceURL' Nothing (Just moduleName) Nothing Nothing (".." </> path) ]
          Nothing -> content


renderSpace :: Int -> String -> Html
renderSpace !_ "" = Html.noHtml
renderSpace !line ('\n':rest) = mconcat
    [ Html.thespan (Html.toHtml '\n')
    , lineAnchor (line + 1)
    , renderSpace (line + 1) rest
    ]
renderSpace line space =
    let (hspace, rest) = span (/= '\n') space
    in (Html.thespan . Html.toHtml) hspace <> renderSpace line rest


lineAnchor :: Int -> Html
lineAnchor line = Html.thespan Html.noHtml ! [ Html.identifier $ hypSrcLineUrl line ]
