-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.Types
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Types (
  SourceURLs, WikiURLs,
  BaseURL,
  withBaseURL,
  LinksInfo,
  Splice,
  Unicode,
  -- * Lucid Shims
  Html,
  makeAttribute,
  noHtml,
  spaceHtml,
  traverse_,
  (+++),
  defList,
  concatHtml,
  module Lucid,
  unordList,
  for_
) where


import Data.Foldable (traverse_, for_)
import Data.Map
import GHC
import qualified System.FilePath as FilePath

import Lucid as Lucid hiding (Html, for_)
import qualified Lucid as L
import Lucid.Base (makeAttribute)
import qualified Data.Text.Lazy as LText

type Html = L.Html ()

unordList :: [Html] -> Html
unordList = ul_ . traverse_ li_

noHtml :: Html
noHtml = mempty

concatHtml :: ToHtml a => [a] -> Html
concatHtml = foldMap toHtml

defList :: (ToHtml a, ToHtml b) => [(a, b)] -> Html
defList items =
  dl_ $ do
    for_ items $ \(dt, dd) -> do
      dt_ $ toHtml dt
      dd_ $ toHtml dd


-- | A @&nbsp@
spaceHtml :: Html
spaceHtml = toHtmlRaw "&nbsp"

(+++) :: Html -> Html -> Html
(+++) = (<>)

-- the base, module and entity URLs for the source code and wiki links.
type SourceURLs = (Maybe FilePath, Maybe FilePath, Map Unit FilePath, Map Unit FilePath)
type WikiURLs = (Maybe FilePath, Maybe FilePath, Maybe FilePath)

-- | base url for loading js, json, css resources.  The default is "."
--
type BaseURL = Maybe String

-- TODO: we shouldn't use 'FilePath.</>'
withBaseURL :: BaseURL -> String -> String
withBaseURL Nothing        uri = uri
withBaseURL (Just baseUrl) uri = baseUrl FilePath.</> uri

-- The URL for source and wiki links
type LinksInfo = (SourceURLs, WikiURLs)

-- Whether something is a splice or not
type Splice = Bool

-- Whether unicode syntax is to be used
type Unicode = Bool
