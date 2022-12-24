{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.Util
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Utils (
  renderToString,

  namedAnchor, linkedAnchor,
  spliceURL, spliceURL',
  groupId,

  (<+>), (<=>), char,
  keyword, punctuate,

  braces, brackets, pabrackets, parens, parenList, ubxParenList, ubxSumList,
  arrow, lollipop, comma, dcolon, dot, darrow, equals, forallSymbol, quote, promoQuote,
  multAnnotation,
  atSign,

  hsep, vcat,

  DetailsState(..), collapseDetails, thesummary,
  collapseToggle, collapseControl,
) where


import Haddock.Utils

import Text.XHtml hiding ( name, title, p, quote )
import qualified Text.XHtml as XHtml

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LText
import GHC              ( SrcSpan(..), srcSpanStartLine, Name )
import GHC.Unit.Module ( Module, ModuleName, moduleName, moduleNameString )
import GHC.Types.Name   ( getOccString, nameOccName, isValOcc )


-- | Replace placeholder string elements with provided values.
--
-- Used to generate URL for customized external paths, usually provided with
-- @--source-module@, @--source-entity@ and related command-line arguments.
--
-- >>> spliceURL Nothing mmod mname Nothing "output/%{MODULE}.hs#%{NAME}"
-- "output/Foo.hs#foo"
spliceURL :: Maybe FilePath -> Maybe Module -> Maybe GHC.Name ->
             Maybe SrcSpan -> LText -> LText
spliceURL mfile mmod = spliceURL' mfile (moduleName <$> mmod)


-- | Same as 'spliceURL' but takes 'ModuleName' instead of 'Module'.
spliceURL' :: Maybe FilePath -> Maybe ModuleName -> Maybe GHC.Name ->
              Maybe SrcSpan -> LText -> LText
spliceURL' maybe_file maybe_mod maybe_name maybe_loc = run
 where
  file = maybe "" LText.pack maybe_file
  mdl = case maybe_mod of
          Nothing           -> ""
          Just m -> LText.pack $ moduleNameString m

  (name, kind) =
    case maybe_name of
      Nothing             -> ("","")
      Just n | isValOcc (nameOccName n) -> (escapeStr (getOccLText n), "v")
             | otherwise -> (escapeStr (getOccLText n), "t")

  line = case maybe_loc of
    Nothing -> ""
    Just span_ ->
      case span_ of
      RealSrcSpan span__ _ ->
        LText.pack $ show $ srcSpanStartLine span__
      UnhelpfulSpan _ -> ""

  run txt =
    case LText.uncons txt of
      Nothing ->
        txt
      Just ('%', (LText.uncons -> Just (c, rest)))
        | c == 'M' ->
            mdl <> run rest
        | c == 'F' ->
            file <> run rest
        | c == 'N' ->
            name <> run rest
        | c == 'K' ->
            kind <> run rest
        | c == 'L' ->
            line <> run rest
        | c == '%' ->
            "%" <> run rest
        | c == '{'  ->
            case rest of
                (LText.stripPrefix "MODULE}" -> Just rest') ->
                    mdl <> run rest'
                (LText.stripPrefix "FILE}" -> Just rest') ->
                    file <> run rest'
                (LText.stripPrefix "NAME}" -> Just rest') ->
                    name <> run rest'
                (LText.stripPrefix "KIND}" -> Just rest') ->
                    kind <> run rest'
                (LText.stripPrefix "MODULE/./" -> Just (LText.uncons -> Just (c', LText.uncons -> Just ('}', rest')))) ->
                    LText.map (\x -> if x == '.' then c' else x) mdl <> run rest'
                (LText.stripPrefix "FILE///" -> Just (LText.uncons -> Just (c', LText.uncons -> Just ('}', rest')))) ->
                    LText.map (\x -> if x == '/' then c' else x) file <> run rest'
                (LText.stripPrefix "LINE}" -> Just rest') ->
                    line <> run rest'
                _ ->
                    error $ "Invalid '{' pattern in spliceURL:" <> LText.unpack txt
        | otherwise ->
            error $ "Invalid pattern in spliceURL: " <> LText.unpack txt
      Just (c, rest) ->
        LText.cons c (run rest)

renderToString :: Bool -> Html -> Builder
renderToString debug html
  | debug = renderHtml html
  | otherwise = showHtml html

{-# INLINE renderToString #-}


hsep :: [Html] -> Html
hsep [] = noHtml
hsep htmls = foldr1 (<+>) htmls

{-# INLINE hsep #-}

-- | Concatenate a series of 'Html' values vertically, with linebreaks in between.
vcat :: [Html] -> Html
vcat [] = noHtml
vcat htmls = foldr1 (\a b -> a+++br+++b) htmls

{-# INLINE vcat #-}

infixr 8 <+>
(<+>) :: Html -> Html -> Html
a <+> b = a +++ sep +++ b
  where
    sep = if isNoHtml a || isNoHtml b then noHtml else toHtml (asText " ")

{-# INLINE (<+>) #-}

-- | Join two 'Html' values together with a linebreak in between.
--   Has 'noHtml' as left identity.
infixr 8 <=>
(<=>) :: Html -> Html -> Html
a <=> b = a +++ sep +++ b
  where
    sep = if isNoHtml a then noHtml else br

{-# INLINE (<=>) #-}


keyword :: Text -> Html
keyword s = thespan ! [theclass "keyword"] << toHtml s


equals, comma :: Html
equals = char '='
comma  = char ','


char :: Char -> Html
char c = toHtml [c]


quote :: Html -> Html
quote h = char '`' +++ h +++ '`'


-- | Promoted type quote (e.g. @'[a, b]@, @'(a, b, c)@).
promoQuote :: Html -> Html
promoQuote h = char '\'' +++ h


parens, brackets, pabrackets, braces :: Html -> Html
parens h        = char '(' +++ h +++ char ')'
brackets h      = char '[' +++ h +++ char ']'
pabrackets h    = toHtml @String "[:" +++ h +++ toHtml @String ":]"
braces h        = char '{' +++ h +++ char '}'

{-# INLINE parens #-}
{-# INLINE brackets #-}
{-# INLINE pabrackets #-}
{-# INLINE braces #-}


punctuate :: Html -> [Html] -> [Html]
punctuate _ []     = []
punctuate h (d0:ds) = go d0 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d +++ h) : go e es

{-# INLINE punctuate #-}

parenList :: [Html] -> Html
parenList = parens . hsep . punctuate comma

{-# INLINE parenList #-}


ubxParenList :: [Html] -> Html
ubxParenList = ubxparens . hsep . punctuate comma

{-# INLINE ubxParenList #-}


ubxSumList :: [Html]  -> Html
ubxSumList = ubxparens . hsep . punctuate (toHtml @String " | ")

{-# INLINE ubxSumList #-}

ubxparens :: Html -> Html
ubxparens h = toHtml @String "(#" <+> h <+> toHtml @String "#)"

{-# INLINE ubxparens #-}


dcolon, arrow, lollipop, darrow, forallSymbol, atSign :: Bool -> Html
dcolon unicode = toHtml @String (if unicode then "∷" else "::")
arrow  unicode = toHtml @String (if unicode then "→" else "->")
lollipop unicode = toHtml @String (if unicode then "⊸" else "%1 ->")
darrow unicode = toHtml @String (if unicode then "⇒" else "=>")
forallSymbol unicode = if unicode then toHtml @String "∀" else keyword "forall"
atSign unicode = toHtml @String (if unicode then "@" else "@")

{-# INLINE dcolon #-}
{-# INLINE  arrow #-}
{-# INLINE  lollipop #-}
{-# INLINE  darrow #-}
{-# INLINE  forallSymbol #-}
{-# INLINE  atSign #-}

multAnnotation :: Html
multAnnotation = toHtml @String "%"

{-# INLINE multAnnotation #-}

dot :: Html
dot = toHtml @String "."

{-# INLINE dot #-}

-- | Generate a named anchor
namedAnchor :: LText -> Html -> Html
namedAnchor n = anchor ! [XHtml.identifier n]

{-# INLINE namedAnchor #-}

linkedAnchor :: LText -> Html -> Html
linkedAnchor n = anchor ! [href ("#" <> n)]

{-# INLINE linkedAnchor #-}

-- | generate an anchor identifier for a group
groupId :: LText -> LText
groupId g = makeAnchorId ("g:" <> g)

{-# INLINE groupId #-}

--
-- A section of HTML which is collapsible.
--

data DetailsState = DetailsOpen | DetailsClosed

collapseDetails :: LText -> DetailsState -> Html -> Html
collapseDetails id_ state = tag "details" ! (identifier id_ : openAttrs)
  where openAttrs = case state of { DetailsOpen -> [emptyAttr "open"]; DetailsClosed -> [] }

{-# INLINE collapseDetails #-}

thesummary :: Html -> Html
thesummary = tag "summary"

{-# INLINE thesummary #-}

-- | Attributes for an area that toggles a collapsed area
collapseToggle :: LText -> LText -> [HtmlAttr]
collapseToggle id_ classes = [ theclass cs, strAttr "data-details-id" id_ ]
  where cs = LText.unwords (LText.words classes ++ ["details-toggle"])

-- | Attributes for an area that toggles a collapsed area,
-- and displays a control.
collapseControl :: LText -> LText -> [HtmlAttr]
collapseControl id_ classes = collapseToggle id_ cs
  where cs = LText.unwords (LText.words classes ++ ["details-toggle-control"])
