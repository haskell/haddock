{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Utils
-- Copyright   :  (c) The University of Glasgow 2001-2002,
--                    Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Utils (

  -- * Filename utilities
  moduleHtmlFile, moduleHtmlFile',
  contentsHtmlFile, indexHtmlFile, indexJsonFile,
  subIndexHtmlFile,
  haddockJsFile, jsQuickJumpFile,
  quickJumpCssFile,

  -- * Anchor and URL utilities
  moduleNameUrl, moduleNameUrl', moduleUrl, moduleUrl',
  nameAnchorId,
  makeAnchorId,

  -- * Miscellaneous utilities
  getProgramName, bye, die, escapeStr,
  writeUtf8File, writeUtf8File', writeUtf8File'', withTempDir,

  -- * HTML cross reference mapping
  html_xrefs_ref, html_xrefs_ref',

  -- * Doc markup
  mkMeta,

  -- * List utilities
  replace,
  spanWith,
  lastMaybe,

  -- * Logging
  parseVerbosity, Verbosity(..),
  out, outMarker, outM,

  -- * System tools
  getProcessID
 ) where


import Documentation.Haddock.Doc (emptyMetaDoc)
import Haddock.Types

import GHC hiding (verbosity)
import GHC.Types.Name

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Catch ( MonadMask, bracket_ )
import Control.Monad.State.Strict (gets)
import Data.ByteString.Builder
import Data.Char ( isAlpha, isAlphaNum, isAscii, ord, chr )
import Data.IORef ( IORef, newIORef, readIORef )
import Data.List ( isSuffixOf, foldl' )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LText
import Debug.Trace (traceMarkerIO)
import Numeric ( showIntAtBase )
import System.Directory ( createDirectory, removeDirectoryRecursive )
import System.Environment ( getProgName )
import System.Exit
import qualified System.FilePath.Posix as HtmlPath
import System.IO ( hPutStr, hSetEncoding, IOMode(..), utf8, withFile )
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.ByteString as B

#ifndef mingw32_HOST_OS
import qualified System.Posix.Internals
#endif

--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------

-- | Parse out a verbosity level. Inspired from Cabal's verbosity parsing.
parseVerbosity :: String -> Either String Verbosity
parseVerbosity "0" = Right Silent
parseVerbosity "1" = Right Normal
parseVerbosity "2" = Right Verbose
parseVerbosity "3" = Right Debug
parseVerbosity "silent"  = return Silent
parseVerbosity "normal"  = return Normal
parseVerbosity "verbose" = return Verbose
parseVerbosity "debug"   = return Debug
parseVerbosity other = Left ("Can't parse verbosity " ++ other)

-- | Print a message to stdout, if it is not too verbose
out :: MonadIO m
    => Verbosity -- ^ program verbosity
    -> Verbosity -- ^ message verbosity
    -> String -> m ()
out progVerbosity msgVerbosity msg
    | msgVerbosity <= progVerbosity = liftIO $ putStrLn msg'
    | otherwise = return ()
  where
    msg' =
      case msgVerbosity of
        Debug -> "HADDOCK DEBUG: " <> msg
        _ -> msg

-- | Emit a marker message to the eventlog, if it is not too verbose. Emitted
-- markers will be visible as markers in, e.g., eventlog2html profiles.
outMarker :: MonadIO m
          => Verbosity -- ^ program verbosity
          -> Verbosity -- ^ message verbosity
          -> String    -- ^ marker message
          -> m ()
outMarker progVerbosity msgVerbosity msg
  | msgVerbosity <= progVerbosity = liftIO $ traceMarkerIO msg
  | otherwise = return ()

-- | Print a message to stdout, if it is not too verbose (inside the interface
-- creation monad)
outM :: MonadIO m => Verbosity -> String -> IfM m ()
outM v msg = do
    verbosity <- gets ifeVerbosity
    liftIO $ out verbosity v msg

--------------------------------------------------------------------------------
-- * Some Utilities
--------------------------------------------------------------------------------



mkMeta :: Doc a -> MDoc a
mkMeta x = emptyMetaDoc { _doc = x }

--------------------------------------------------------------------------------
-- * Filename mangling functions stolen from s main/DriverUtil.lhs.
--------------------------------------------------------------------------------

baseName :: ModuleName -> FilePath
baseName = map (\c -> if c == '.' then '-' else c) . moduleNameString


moduleHtmlFile :: Module -> FilePath
moduleHtmlFile mdl =
  case Map.lookup mdl html_xrefs of
    Nothing  -> baseName mdl' ++ ".html"
    Just fp0 -> HtmlPath.joinPath [fp0, baseName mdl' ++ ".html"]
  where
   mdl' = moduleName mdl


moduleHtmlFile' :: ModuleName -> FilePath
moduleHtmlFile' mdl =
  case Map.lookup mdl html_xrefs' of
    Nothing  -> baseName mdl ++ ".html"
    Just fp0 -> HtmlPath.joinPath [fp0, baseName mdl ++ ".html"]


contentsHtmlFile, indexHtmlFile, indexJsonFile, indexJsonContentsFile :: String
contentsHtmlFile = "index.html"
indexHtmlFile = "doc-index.html"
indexJsonFile = "doc-index.json"
indexJsonContentsFile = "doc-index-contents.txt"


subIndexHtmlFile :: String -> String
subIndexHtmlFile ls = "doc-index-" ++ b ++ ".html"
   where b | all isAlpha ls = ls
           | otherwise = concatMap (show . ord) ls


-------------------------------------------------------------------------------
-- * Anchor and URL utilities
--
-- NB: Anchor IDs, used as the destination of a link within a document must
-- conform to XML's NAME production. That, taken with XHTML and HTML 4.01's
-- various needs and compatibility constraints, means these IDs have to match:
--      [A-Za-z][A-Za-z0-9:_.-]*
-- Such IDs do not need to be escaped in any way when used as the fragment part
-- of a URL. Indeed, %-escaping them can lead to compatibility issues as it
-- isn't clear if such fragment identifiers should, or should not be unescaped
-- before being matched with IDs in the target document.
-------------------------------------------------------------------------------


moduleUrl :: Module -> Text
moduleUrl = LText.pack . moduleHtmlFile

moduleUrl' :: ModuleName -> Text
moduleUrl' = LText.pack . moduleHtmlFile'

moduleNameUrl :: Module -> OccName -> Text
moduleNameUrl mdl n = moduleUrl mdl <> ('#' `LText.cons` nameAnchorId n)

moduleNameUrl' :: ModuleName -> OccName -> Text
moduleNameUrl' mdl n = moduleUrl' mdl <> ('#' `LText.cons` nameAnchorId n)


nameAnchorId :: OccName -> Text
nameAnchorId name =
    makeAnchorId
      (              prefix
        `LText.cons` ':'
        `LText.cons` LText.pack (occNameString name)
      )
  where
    prefix
      | isValOcc name = 'v'
      | otherwise     = 't'

-- | Takes an arbitrary string and makes it a valid anchor ID. The mapping is
-- identity preserving.
--
-- The 'Text' is first unpacked to a 'String' to avoid the O(n) cons operations.
makeAnchorId :: Text -> Text
makeAnchorId xs = LText.pack $
    case LText.unpack xs of
      ""     -> ""
      (y:ys) -> escape isAlpha y (foldr (escape isLegal) [] ys)
  where
    escape :: (Char -> Bool) -> Char -> String -> String
    escape p c acc
      | p c       = c : acc
      | otherwise = ('-' : show (ord c)) ++ ('-' : acc)

    isLegal :: Char -> Bool
    isLegal ':' = True
    isLegal '_' = True
    isLegal '.' = True
    isLegal c = isAscii c && isAlphaNum c
    -- NB: '-' is legal in IDs, but we use it as the escape char

-------------------------------------------------------------------------------
-- * Files we need to copy from our $libdir
-------------------------------------------------------------------------------


haddockJsFile :: String
haddockJsFile = "haddock-bundle.min.js"

jsQuickJumpFile :: String
jsQuickJumpFile = "quick-jump.min.js"

quickJumpCssFile :: String
quickJumpCssFile = "quick-jump.css"

-------------------------------------------------------------------------------
-- * Misc.
-------------------------------------------------------------------------------


getProgramName :: IO String
getProgramName = fmap (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str


bye :: String -> IO a
bye s = putStr s >> exitSuccess

escapeStr :: String -> String
escapeStr = escapeURIString isUnreserved


-- Following few functions are copy'n'pasted from Network.URI module
-- to avoid depending on the network lib, since doing so gives a
-- circular build dependency between haddock and network
-- (at least if you want to build network with haddock docs)
escapeURIChar :: (Char -> Bool) -> Char -> String
escapeURIChar p c
    | p c       = [c]
    | otherwise = '%' : myShowHex (ord c) ""
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 toChrHex n r of
            []  -> "00"
            [a] -> ['0',a]
            cs  -> cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))


escapeURIString :: (Char -> Bool) -> String -> String
escapeURIString = concatMap . escapeURIChar


isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNumChar c || (c `elem` "-_.~")


isAlphaChar, isDigitChar, isAlphaNumChar :: Char -> Bool
isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
isDigitChar c    = c >= '0' && c <= '9'
isAlphaNumChar c = isAlphaChar c || isDigitChar c

writeUtf8File :: FilePath -> String -> IO ()
writeUtf8File filepath contents = withFile filepath WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h contents

-- | Utility to write output to UTF-8 encoded files.
--
-- The problem with 'writeFile' is that it picks up its 'TextEncoding' from
-- 'getLocaleEncoding', and on some platforms (like Windows) this default
-- encoding isn't enough for the characters we want to write.
writeUtf8File' :: FilePath -> Builder -> IO ()
writeUtf8File' filepath contents = withFile filepath WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutBuilder h contents

writeUtf8File'' :: FilePath -> B.ByteString -> IO ()
writeUtf8File'' filepath contents = withFile filepath WriteMode $ \h -> do
  hSetEncoding h utf8
  B.hPutStr h contents

withTempDir :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withTempDir dir = bracket_ (liftIO $ createDirectory dir)
                           (liftIO $ removeDirectoryRecursive dir)

-----------------------------------------------------------------------------
-- * HTML cross references
--
-- For each module, we need to know where its HTML documentation lives
-- so that we can point hyperlinks to it.  It is extremely
-- inconvenient to plumb this information to all the places that need
-- it (basically every function in HaddockHtml), and furthermore the
-- mapping is constant for any single run of Haddock.  So for the time
-- being I'm going to use a write-once global variable.
-----------------------------------------------------------------------------


{-# NOINLINE html_xrefs_ref #-}
html_xrefs_ref :: IORef (Map Module FilePath)
html_xrefs_ref = unsafePerformIO (newIORef (error "module_map"))


{-# NOINLINE html_xrefs_ref' #-}
html_xrefs_ref' :: IORef (Map ModuleName FilePath)
html_xrefs_ref' = unsafePerformIO (newIORef (error "module_map"))


{-# NOINLINE html_xrefs #-}
html_xrefs :: Map Module FilePath
html_xrefs = unsafePerformIO (readIORef html_xrefs_ref)


{-# NOINLINE html_xrefs' #-}
html_xrefs' :: Map ModuleName FilePath
html_xrefs' = unsafePerformIO (readIORef html_xrefs_ref')


-----------------------------------------------------------------------------
-- * List utils
-----------------------------------------------------------------------------


replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)


spanWith :: (a -> Maybe b) -> [a] -> ([b],[a])
spanWith _ [] = ([],[])
spanWith p xs@(a:as)
  | Just b <- p a = let (bs,cs) = spanWith p as in (b:bs,cs)
  | otherwise     = ([],xs)

-- | 'Nothing' for empty lists, 'Just' @x@ for non-empty lists whose last
-- element is @x@.
lastMaybe :: [a] -> Maybe a
lastMaybe = foldl' (\_ -> pure) Nothing

-----------------------------------------------------------------------------
-- * System tools
-----------------------------------------------------------------------------


#ifdef mingw32_HOST_OS
foreign import ccall unsafe "_getpid" getProcessID :: IO Int -- relies on Int == Int32 on Windows
#else
getProcessID :: IO Int
getProcessID = fmap fromIntegral System.Posix.Internals.c_getpid
#endif
