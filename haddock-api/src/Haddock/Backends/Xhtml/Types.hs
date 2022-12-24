{-# LANGUAGE OverloadedStrings #-}

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
  LText,
) where


import Data.Map
import GHC
import qualified System.FilePath as FilePath
import Text.XHtml (LText)
import qualified Data.Text.Lazy as LText


-- the base, module and entity URLs for the source code and wiki links.
type SourceURLs = (Maybe LText, Maybe LText, Map Unit LText, Map Unit LText)
type WikiURLs = (Maybe LText, Maybe LText, Maybe LText)

-- | base url for loading js, json, css resources.  The default is "."
--
type BaseURL = Maybe LText

withBaseURL :: BaseURL -> LText -> LText
withBaseURL Nothing        uri = uri
withBaseURL (Just baseUrl) uri = baseUrl <> "/" <> uri

-- The URL for source and wiki links
type LinksInfo = (SourceURLs, WikiURLs)

-- Whether something is a splice or not
type Splice = Bool

-- Whether unicode syntax is to be used
type Unicode = Bool
