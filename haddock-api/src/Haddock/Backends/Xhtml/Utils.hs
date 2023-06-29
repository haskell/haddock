{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
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
  renderToBuilder,

  textHtml, (<<<),

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

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LText
import Data.ByteString.Builder
import Text.XHtml hiding ( name, title, p, quote )
import qualified Text.XHtml as XHtml

import GHC              ( SrcSpan(..), srcSpanStartLine, Name )
import GHC.Unit.Module ( Module, ModuleName, moduleName, moduleNameString )
import GHC.Types.Name   ( getOccString, nameOccName, isValOcc )

--
-- 'Html' functions specialised to 'Text'
--

textHtml :: Text -> Html
textHtml = toHtml

(<<<) :: (Html -> a) -> Text -> a
(<<<) = (<<)

infixr 7 <<<

-- | Replace placeholder string elements with provided values.
--
-- Used to generate URL for customized external paths, usually provided with
-- @--source-module@, @--source-entity@ and related command-line arguments.
--
-- >>> spliceURL mmod mname Nothing "output/%{MODULE}.hs#%{NAME}"
-- "output/Foo.hs#foo"
spliceURL :: Maybe Module -> Maybe GHC.Name ->
             Maybe SrcSpan -> String -> String
spliceURL mmod = spliceURL' (moduleName <$> mmod)


-- | Same as 'spliceURL' but takes 'ModuleName' instead of 'Module'.
spliceURL' :: Maybe ModuleName -> Maybe GHC.Name ->
              Maybe SrcSpan -> String -> String
spliceURL' maybe_mod maybe_name maybe_loc = run
 where
  mdl = case maybe_mod of
          Nothing           -> ""
          Just m -> moduleNameString m

  (name, kind) =
    case maybe_name of
      Nothing             -> ("","")
      Just n | isValOcc (nameOccName n) -> (escapeStr (getOccString n), "v")
             | otherwise -> (escapeStr (getOccString n), "t")

  line = case maybe_loc of
    Nothing -> ""
    Just span_ ->
      case span_ of
      RealSrcSpan span__ _ ->
        show $ srcSpanStartLine span__
      UnhelpfulSpan _ -> ""

  run "" = ""
  run ('%':'M':rest) = mdl  ++ run rest
  run ('%':'N':rest) = name ++ run rest
  run ('%':'K':rest) = kind ++ run rest
  run ('%':'L':rest) = line ++ run rest
  run ('%':'%':rest) = '%'   : run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'}':rest) = mdl  ++ run rest
  run ('%':'{':'N':'A':'M':'E':'}':rest)         = name ++ run rest
  run ('%':'{':'K':'I':'N':'D':'}':rest)         = kind ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'/':'.':'/':c:'}':rest) =
    map (\x -> if x == '.' then c else x) mdl ++ run rest

  run ('%':'{':'L':'I':'N':'E':'}':rest)         = line ++ run rest

  run (c:rest) = c : run rest


renderToBuilder :: Bool -> Html -> Builder
renderToBuilder debug html
  | debug = renderHtml html
  | otherwise = showHtml html


hsep :: [Html] -> Html
hsep [] = noHtml
hsep htmls = foldr1 (<+>) htmls

-- | Concatenate a series of 'Html' values vertically, with linebreaks in between.
vcat :: [Html] -> Html
vcat [] = noHtml
vcat htmls = foldr1 (\a b -> a+++br+++b) htmls


infixr 8 <+>
(<+>) :: Html -> Html -> Html
a <+> b
  | isNoHtml a = b
  | isNoHtml b = a
  | otherwise = a +++ textHtml " " +++ b

-- | Join two 'Html' values together with a linebreak in between.
--   Has 'noHtml' as left identity.
infixr 8 <=>
(<=>) :: Html -> Html -> Html
a <=> b = a +++ sep +++ b
  where
    sep = if isNoHtml a then noHtml else br


keyword :: String -> Html
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
pabrackets h    = textHtml "[:" +++ h +++ textHtml ":]"
braces h        = char '{' +++ h +++ char '}'


punctuate :: Html -> [Html] -> [Html]
punctuate _ []     = []
punctuate h (d0:ds) = go d0 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d +++ h) : go e es


parenList :: [Html] -> Html
parenList = parens . hsep . punctuate comma


ubxParenList :: [Html] -> Html
ubxParenList = ubxparens . hsep . punctuate comma


ubxSumList :: [Html]  -> Html
ubxSumList = ubxparens . hsep . punctuate (textHtml " | ")


ubxparens :: Html -> Html
ubxparens h = textHtml "(#" <+> h <+> textHtml "#)"


dcolon, arrow, lollipop, darrow, forallSymbol, atSign :: Bool -> Html
dcolon unicode = textHtml (if unicode then "∷" else "::")
arrow  unicode = textHtml (if unicode then "→" else "->")
lollipop unicode = textHtml (if unicode then "⊸" else "%1 ->")
darrow unicode = textHtml (if unicode then "⇒" else "=>")
forallSymbol unicode = if unicode then textHtml "∀" else keyword "forall"
atSign unicode = textHtml (if unicode then "@" else "@")

multAnnotation :: Html
multAnnotation = textHtml "%"

dot :: Html
dot = textHtml "."


-- | Generate a named anchor
namedAnchor :: Text -> Html -> Html
namedAnchor n = anchor ! [XHtml.identifier n]


linkedAnchor :: Text -> Html -> Html
linkedAnchor n = anchor ! [href ('#' `LText.cons` n)]


-- | generate an anchor identifier for a group
groupId :: Text -> Text
groupId g = makeAnchorId ("g:" <> g)

--
-- A section of HTML which is collapsible.
--

data DetailsState = DetailsOpen | DetailsClosed

collapseDetails :: Text -> DetailsState -> Html -> Html
collapseDetails id_ state = tag "details" ! (identifier id_ : openAttrs)
  where openAttrs = case state of { DetailsOpen -> [emptyAttr "open"]; DetailsClosed -> [] }

thesummary :: Html -> Html
thesummary = tag "summary"

-- | Attributes for an area that toggles a collapsed area
collapseToggle :: Text -> Text -> [HtmlAttr]
collapseToggle id_ classes = [ theclass cs, strAttr "data-details-id" id_ ]
  where cs = LText.unwords (LText.words classes <> ["details-toggle"])

-- | Attributes for an area that toggles a collapsed area,
-- and displays a control.
collapseControl :: Text -> Text -> [HtmlAttr]
collapseControl id_ classes = collapseToggle id_ cs
  where cs = LText.unwords (LText.words classes <> ["details-toggle-control"])
