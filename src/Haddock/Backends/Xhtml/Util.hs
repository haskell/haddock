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
module Haddock.Backends.Xhtml.Util (
  renderToString,
  
  namedAnchor, linkedAnchor,
  spliceURL,
  
  (<+>), char, empty,
  keyword, punctuate,
  
  braces, brackets, pabrackets, parens, parenList, ubxParenList,
  arrow, comma, dcolon, dot, darrow, equals, forallSymbol, quote,
  
  tda, emptyTable, s8,
  abovesSep, hsep,
  
  collapsebutton, collapseId, collapsed,
  documentCharacterEncoding, styleSheet
) where

import Haddock.GhcUtils
import Haddock.Utils

import Data.Maybe

import Text.XHtml hiding ( name, title, p, quote )
import qualified Text.XHtml as Html

import GHC      ( SrcSpan, srcSpanStartLine, Name )
import Module   ( Module )
import Name     ( getOccString, nameOccName, isValOcc )


spliceURL :: Maybe FilePath -> Maybe Module -> Maybe GHC.Name -> 
             Maybe SrcSpan -> String -> String
spliceURL maybe_file maybe_mod maybe_name maybe_loc url = run url
 where
  file = fromMaybe "" maybe_file
  mdl = case maybe_mod of
          Nothing           -> ""
          Just m -> moduleString m
  
  (name, kind) =
    case maybe_name of
      Nothing             -> ("","")
      Just n | isValOcc (nameOccName n) -> (escapeStr (getOccString n), "v")
             | otherwise -> (escapeStr (getOccString n), "t")

  line = case maybe_loc of
    Nothing -> ""
    Just span_ -> show $ srcSpanStartLine span_

  run "" = ""
  run ('%':'M':rest) = mdl  ++ run rest
  run ('%':'F':rest) = file ++ run rest
  run ('%':'N':rest) = name ++ run rest
  run ('%':'K':rest) = kind ++ run rest
  run ('%':'L':rest) = line ++ run rest
  run ('%':'%':rest) = "%" ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'}':rest) = mdl  ++ run rest
  run ('%':'{':'F':'I':'L':'E':'}':rest)         = file ++ run rest
  run ('%':'{':'N':'A':'M':'E':'}':rest)         = name ++ run rest
  run ('%':'{':'K':'I':'N':'D':'}':rest)         = kind ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'/':'.':'/':c:'}':rest) =
    map (\x -> if x == '.' then c else x) mdl ++ run rest

  run ('%':'{':'F':'I':'L':'E':'/':'/':'/':c:'}':rest) =
    map (\x -> if x == '/' then c else x) file ++ run rest

  run ('%':'{':'L':'I':'N':'E':'}':rest)         = line ++ run rest

  run (c:rest) = c : run rest


renderToString :: Html -> String
renderToString = showHtml     -- for production
--renderToString = prettyHtml   -- for debugging

hsep :: [Html] -> Html
hsep [] = noHtml
hsep htmls = foldr1 (\a b -> a+++" "+++b) htmls

infixr 8 <+>
(<+>) :: Html -> Html -> Html
a <+> b = a +++ toHtml " " +++ b

keyword :: String -> Html
keyword s = thespan ! [theclass "keyword"] << toHtml s

equals, comma :: Html
equals = char '='
comma  = char ','

char :: Char -> Html
char c = toHtml [c]

empty :: Html
empty  = noHtml


quote :: Html -> Html
quote h = char '`' +++ h +++ '`'


parens, brackets, pabrackets, braces :: Html -> Html
parens h        = char '(' +++ h +++ char ')'
brackets h      = char '[' +++ h +++ char ']'
pabrackets h    = toHtml "[:" +++ h +++ toHtml ":]"
braces h        = char '{' +++ h +++ char '}'

punctuate :: Html -> [Html] -> [Html]
punctuate _ []     = []
punctuate h (d0:ds) = go d0 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d +++ h) : go e es

abovesSep :: HtmlTable -> [HtmlTable] -> HtmlTable
abovesSep _ []      = emptyTable
abovesSep h (d0:ds) = go d0 ds
                   where
                     go d [] = d
                     go d (e:es) = d </> h </> go e es

parenList :: [Html] -> Html
parenList = parens . hsep . punctuate comma

ubxParenList :: [Html] -> Html
ubxParenList = ubxparens . hsep . punctuate comma

ubxparens :: Html -> Html
ubxparens h = toHtml "(#" +++ h +++ toHtml "#)"


tda :: [HtmlAttr] -> Html -> HtmlTable
tda as = cell . (td ! as)

emptyTable :: HtmlTable
emptyTable = cell noHtml

onclick :: String -> HtmlAttr
onclick = strAttr "onclick"


dcolon, arrow, darrow, forallSymbol :: Bool -> Html
dcolon unicode = toHtml (if unicode then "∷" else "::")
arrow  unicode = toHtml (if unicode then "→" else "->")
darrow unicode = toHtml (if unicode then "⇒" else "=>")
forallSymbol unicode = if unicode then toHtml "∀" else keyword "forall"


dot :: Html
dot = toHtml "."


s8 :: HtmlTable
s8  = tda [ theclass "s8" ]  << noHtml


-- | Generate a named anchor
--
-- This actually generates two anchor tags, one with the name unescaped, and one
-- with the name URI-escaped. This is needed because Opera 9.52 (and later
-- versions) needs the name to be unescaped, while IE 7 needs it to be escaped.
-- 
namedAnchor :: String -> Html -> Html
namedAnchor n c = anchor ! [Html.name n] << noHtml +++
                  anchor ! [Html.name (escapeStr n)] << c


--
-- A section of HTML which is collapsible via a +/- button.
--

-- TODO: Currently the initial state is non-collapsed. Change the 'minusFile'
-- below to a 'plusFile' and the 'display:block;' to a 'display:none;' when we
-- use cookies from JavaScript to have a more persistent state.

collapsebutton :: String -> Html
collapsebutton id_ = 
  image ! [ src minusFile, theclass "coll", onclick ("toggle(this,'" ++ id_ ++ "')"), alt "show/hide" ]

collapsed :: (HTML a) => (Html -> Html) -> String -> a -> Html
collapsed fn id_ html =
  fn ! [identifier id_, thestyle "display:block;"] << html

-- A quote is a valid part of a Haskell identifier, but it would interfere with
-- the ECMA script string delimiter used in collapsebutton above.
collapseId :: Name -> String
collapseId nm = "i:" ++ escapeStr (getOccString nm)

linkedAnchor :: String -> Html -> Html
linkedAnchor frag = anchor ! [href hr_]
   where hr_ | null frag = ""
             | otherwise = '#': escapeStr frag

documentCharacterEncoding :: Html
documentCharacterEncoding =
   meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"]

styleSheet :: Html
styleSheet =
   (thelink ! [href cssFile, rel "stylesheet", thetype "text/css"]) noHtml
