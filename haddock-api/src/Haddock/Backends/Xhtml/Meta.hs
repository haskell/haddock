module Haddock.Backends.Xhtml.Meta where

import Haddock.Utils.Json
import Haddock.Version
import qualified Data.Text as Text

import Data.ByteString.Builder (hPutBuilder)
import System.FilePath ((</>))
import System.IO (withFile, IOMode (WriteMode))

-- | Everytime breaking changes to the Quckjump api
-- happen this needs to be modified.
quickjumpVersion :: Int
quickjumpVersion = 1

-- | Writes a json encoded file containing additional
-- information about the generated documentation. This
-- is useful for external tools (e.g., Hackage).
writeHaddockMeta :: FilePath -> Bool -> IO ()
writeHaddockMeta odir withQuickjump = do
  let
    meta_json :: Value
    meta_json = object (concat [
        [ Text.pack "haddock_version"   .= Text.pack projectVersion ]
      , [ Text.pack "quickjump_version" .= quickjumpVersion | withQuickjump ]
      ])

  withFile (odir </> "meta.json") WriteMode $ \h ->
    hPutBuilder h (encodeToBuilder meta_json)
