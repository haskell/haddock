{-# LANGUAGE OverloadedStrings #-}
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

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL


import Haddock.Utils

import Data.Maybe

import Lucid
import Lucid.Base (makeElement, makeAttributes)

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
             Maybe SrcSpan -> String -> String
spliceURL mfile mmod = spliceURL' mfile (moduleName <$> mmod)


-- | Same as 'spliceURL' but takes 'ModuleName' instead of 'Module'.
spliceURL' :: Maybe FilePath -> Maybe ModuleName -> Maybe GHC.Name ->
              Maybe SrcSpan -> String -> String
spliceURL' maybe_file maybe_mod maybe_name maybe_loc = run
 where
  file = fromMaybe "" maybe_file
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
    Just span' ->
      case span' of
      RealSrcSpan span__ _ ->
        show $ srcSpanStartLine span__
      UnhelpfulSpan _ -> ""

  run "" = ""
  run ('%':'M':rest) = mdl  ++ run rest
  run ('%':'F':rest) = file ++ run rest
  run ('%':'N':rest) = name ++ run rest
  run ('%':'K':rest) = kind ++ run rest
  run ('%':'L':rest) = line ++ run rest
  run ('%':'%':rest) = '%'   : run rest

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


renderToString :: Bool -> Html () -> String
renderToString _ html = TL.unpack $ renderText html

hsep :: [Html ()] -> Html ()
hsep [] = pure ()
hsep htmls = foldr1 (<+>) htmls

-- | Concatenate a series of 'Html' values vertically, with linebreaks in between.
vcat :: [Html ()] -> Html ()
vcat [] = pure ()
vcat htmls = foldr1 (\a b -> a<>br_ []<>b) htmls


infixr 8 <+>
(<+>) :: Html () -> Html () -> Html ()
a <+> b = a <> sep <> b
  where
    sep = if isNoHtml a || isNoHtml b then pure () else toHtml (" " :: Text)

-- | Join two 'Html' values together with a linebreak in between.
--   Has 'pure ()' as left identity.
infixr 8 <=>
(<=>) :: Html () -> Html () -> Html ()
a <=> b = a <> sep <> b
  where
    sep = if isNoHtml a then pure () else br_ []


keyword :: String -> Html ()
keyword s = span_ [class_ "keyword"] (toHtml s)


equals, comma :: Html ()
equals = char '='
comma  = char ','


char :: Char -> Html ()
char c = toHtml [c]


quote :: Html () -> Html ()
quote h = char '`' <> h <> "`"


-- | Promoted type quote (e.g. @'[a, b]@, @'(a, b, c)@).
promoQuote :: Html () -> Html ()
promoQuote h = char '\'' <> h


parens, brackets, pabrackets, braces :: Html () -> Html ()
parens h        = char '(' <> h <> char ')'
brackets h      = char '[' <> h <> char ']'
pabrackets h    = toHtml ("[:" :: Text) <> h <> toHtml (":]" :: Text)
braces h        = char '{' <> h <> char '}'


punctuate :: Html () -> [Html ()] -> [Html ()]
punctuate _ []     = []
punctuate h (d0:ds) = go d0 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> h) : go e es


parenList :: [Html ()] -> Html ()
parenList = parens . hsep . punctuate comma


ubxParenList :: [Html ()] -> Html ()
ubxParenList = ubxparens . hsep . punctuate comma


ubxSumList :: [Html ()]  -> Html ()
ubxSumList = ubxparens . hsep . punctuate (toHtml (" | " :: Text))


ubxparens :: Html () -> Html ()
ubxparens h = toHtml( "(#"  :: Text) <+> h <+> toHtml ("#)" :: Text)


dcolon, arrow, lollipop, darrow, forallSymbol, atSign :: Bool -> Html ()
dcolon unicode = toHtml (if unicode then "∷" else "::" :: Text)
arrow  unicode = toHtml (if unicode then "→" else "->"  :: Text)
lollipop unicode = toHtml (if unicode then "⊸" else "%1 ->"  :: Text)
darrow unicode = toHtml (if unicode then "⇒" else "=>"  :: Text)
forallSymbol unicode = if unicode then toHtml ("∀"   :: Text) else keyword "forall"
atSign unicode = toHtml (if unicode then "@" else "@"  :: Text)

multAnnotation :: Html ()
multAnnotation = toHtml ("%"   :: Text)

dot :: Html ()
dot = toHtml ("."   :: Text)


-- | Generate a named anchor
namedAnchor :: Text -> Attributes -> Html () -> Html ()
namedAnchor n attr = a_ [id_ n, attr]


linkedAnchor :: Text -> Html () -> Html ()
linkedAnchor n = a_ [href_ (Text.cons '#' n)]


-- | generate an anchor identifier for a group
groupId :: String -> String
groupId g = makeAnchorId ("g:" ++ g)

--
-- A section of HTML which is collapsible.
--

data DetailsState = DetailsOpen | DetailsClosed

collapseDetails :: Text -> DetailsState -> Html () -> Html ()
collapseDetails id' state = makeElement "details" (id_ id' : openAttrs)
  where openAttrs = case state of { DetailsOpen -> [emptyAttr "open"]; DetailsClosed -> [] }

thesummary :: Html () -> Html ()
thesummary = makeElement "summary" []

-- | Attributes for an area that toggles a collapsed area
collapseToggle :: Text -> Text -> [Attributes]
collapseToggle id' classes = [ class_ cs, makeAttributes "data-details-id" id' ]
  where cs = Text.unwords (Text.words classes ++ ["details-toggle"])

-- | Attributes for an area that toggles a collapsed area,
-- and displays a control.
collapseControl :: Text -> Text -> [Attributes]
collapseControl id' classes = collapseToggle id' cs
  where cs = Text.unwords (Text.words classes ++ ["details-toggle-control"])

isNoHtml :: Html () -> Bool
isNoHtml = (=="") . renderBS

emptyAttr :: Text -> Attributes
emptyAttr s = makeAttributes s s