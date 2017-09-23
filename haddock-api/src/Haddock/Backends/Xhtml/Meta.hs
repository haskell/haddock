module Haddock.Backends.Xhtml.Meta where

import Haddock.Utils.Json
import Haddock.Version

import Data.ByteString.Builder (hPutBuilder)
import System.FilePath ((</>))
import System.IO (withFile, IOMode (WriteMode))

-- | Everytime someone makes a breaking change to QuickJump this
-- has to be incremented. This is used to guarantee compatibility
-- for external tools (e.g. hackage).
quickjumpVersion :: Int
quickjumpVersion = 1

-- | Writes a json encoded file containing additional
-- information about the generated documentation. This
-- is useful for external tools (e.g. hackage).
writeHaddockMeta :: Bool     -- ^ Is QuickJump enabled?
                 -> FilePath -- ^ Output directory
                 -> IO ()
writeHaddockMeta withQuickJump odir = do
  let
    meta_json :: Value
    meta_json = object $ concat [
        [ "haddock_version"   .= String projectVersion            ]
      , [ "quickjump_version" .= quickjumpVersion | withQuickJump ]
      ]

  withFile (odir </> "meta.json") WriteMode $ \h ->
    hPutBuilder h (encodeToBuilder meta_json)
