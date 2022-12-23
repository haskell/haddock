{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

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
  moduleNameUrl, moduleNameUrl', moduleUrl,
  nameAnchorId,
  makeAnchorId,

  -- * Miscellaneous utilities
  getProgramName, bye, die, escapeStr,
  writeUtf8File, withTempDir,

  -- * HTML cross reference mapping
  html_xrefs_ref, html_xrefs_ref',

  -- * Doc markup
  mkMeta,

  -- * List utilities
  replace,
  spanWith,
  ordNub,

  -- * Logging
  parseVerbosity, Verbosity(..), silent, normal, verbose, deafening,
  out,

  -- * System tools
  getProcessID,

  asText, builderToText, textToBuilder, builderToString,
  Builder, stringUtf8, lazyTextToBuilder,
  SText, LText,
  occNameLText, moduleNameLText,
  getOccLText, fastStringToLText,
 ) where


import qualified Data.Set as Set
import Documentation.Haddock.Doc (emptyMetaDoc)
import Haddock.Types

import GHC
import GHC.Types.Name

import Data.String (IsString(..))
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Catch ( MonadMask, bracket_ )
import Data.Char ( isAlpha, isAlphaNum, isAscii, ord, chr )
import Numeric ( showIntAtBase )
import Data.Map ( Map )
import qualified Data.Map as Map hiding ( Map )
import Data.IORef ( IORef, newIORef, readIORef )
import Data.List ( isSuffixOf )
import System.Environment ( getProgName )
import System.Exit
import System.Directory ( createDirectory, removeDirectoryRecursive )
import System.IO.Unsafe ( unsafePerformIO )
import qualified System.FilePath.Posix as HtmlPath
import Data.ByteString.Builder as Builder
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Lazy  as BSL
import GHC.Data.FastString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Text.XHtml (LText)
import GHC.Unit.Module.Name

#ifndef mingw32_HOST_OS
import qualified System.Posix.Internals
#endif

type SText = Text.Text

asText :: LText.Text -> LText.Text
asText = id

builderToText :: Builder -> Text.Text
builderToText = Text.decodeUtf8 . BS.toStrict . toLazyByteString

builderToString :: Builder -> String
builderToString = Text.unpack . builderToText

textToBuilder :: Text.Text -> Builder
textToBuilder = byteString . Text.encodeUtf8

lazyTextToBuilder :: LText.Text -> Builder
lazyTextToBuilder = lazyByteString . LText.encodeUtf8

--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------

data Verbosity = Silent | Normal | Verbose | Deafening
  deriving (Eq, Ord, Enum, Bounded, Show)

silent, normal, verbose, deafening :: Verbosity
silent    = Silent
normal    = Normal
verbose   = Verbose
deafening = Deafening

-- | Parse out a verbosity level. Inspired from Cabal's verbosity parsing.
parseVerbosity :: String -> Either String Verbosity
parseVerbosity "0" = Right Silent
parseVerbosity "1" = Right Normal
parseVerbosity "2" = Right Silent
parseVerbosity "3" = Right Deafening
parseVerbosity "silent"    = return Silent
parseVerbosity "normal"    = return Normal
parseVerbosity "verbose"   = return Verbose
parseVerbosity "debug"     = return Deafening
parseVerbosity "deafening" = return Deafening
parseVerbosity other = Left ("Can't parse verbosity " ++ other)

-- | Print a message to stdout, if it is not too verbose
out :: MonadIO m
    => Verbosity -- ^ program verbosity
    -> Verbosity -- ^ message verbosity
    -> String -> m ()
out progVerbosity msgVerbosity msg
  | msgVerbosity <= progVerbosity = liftIO $ putStrLn msg
  | otherwise = return ()


--------------------------------------------------------------------------------
-- * Some Utilities
--------------------------------------------------------------------------------

ordNub :: Ord a => [a] -> [a]
ordNub = go mempty
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs


mkMeta :: Doc a -> MDoc a
mkMeta x = emptyMetaDoc { _doc = x }

--------------------------------------------------------------------------------
-- * Filename mangling functions stolen from s main/DriverUtil.lhs.
--------------------------------------------------------------------------------

baseName :: ModuleName -> FilePath
baseName = map (\c -> if c == '.' then '-' else c) . moduleNameString


moduleHtmlFile :: Module -> LText
moduleHtmlFile mdl = LText.pack $
  case Map.lookup mdl html_xrefs of
    Nothing  -> baseName mdl' ++ ".html"
    Just fp0 -> HtmlPath.joinPath [fp0, baseName mdl' ++ ".html"]
  where
   mdl' = moduleName mdl


moduleHtmlFile' :: ModuleName -> LText
moduleHtmlFile' mdl = LText.pack $
  case Map.lookup mdl html_xrefs' of
    Nothing  -> baseName mdl ++ ".html"
    Just fp0 -> HtmlPath.joinPath [fp0, baseName mdl ++ ".html"]


contentsHtmlFile, indexHtmlFile, indexJsonFile :: IsString s => s
contentsHtmlFile = "index.html"
indexHtmlFile = "doc-index.html"
indexJsonFile = "doc-index.json"


subIndexHtmlFile :: LText -> LText
subIndexHtmlFile ls = "doc-index-" <> b <> ".html"
   where b | LText.all isAlpha ls = ls
           | otherwise = LText.concatMap (LText.pack . show . ord) ls


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


moduleUrl :: Module -> LText
moduleUrl = moduleHtmlFile


moduleNameUrl :: Module -> OccName -> LText
moduleNameUrl mdl n = moduleUrl mdl <> "#" <> nameAnchorId n


moduleNameUrl' :: ModuleName -> OccName -> LText
moduleNameUrl' mdl n = moduleHtmlFile' mdl <> "#" <> nameAnchorId n


nameAnchorId :: OccName -> LText
nameAnchorId name = makeAnchorId (prefix <> occNameLText name)
 where prefix | isValOcc name = "v:"
              | otherwise     = "t:"

occNameLText :: OccName -> LText
occNameLText = fastStringToLText . occNameFS
{-# INLINE occNameLText #-}

getOccLText :: NamedThing a => a -> LText
getOccLText = fastStringToLText . getOccFS
{-# INLINE getOccLText #-}

fastStringToLText :: FastString -> LText
fastStringToLText =
  LText.decodeUtf8 . BSL.fromStrict . bytesFS
{-# INLINE fastStringToLText #-}

moduleNameLText :: ModuleName -> LText
moduleNameLText = fastStringToLText . moduleNameFS
{-# INLINE moduleNameLText #-}


-- | Takes an arbitrary string and makes it a valid anchor ID. The mapping is
-- identity preserving.
makeAnchorId :: LText -> LText
makeAnchorId txt =
  case LText.uncons txt of
    Just (f, r) ->
      LText.pack (escape isAlpha f) <> LText.concatMap (LText.pack . escape isLegal) r
    Nothing ->
      txt
  where
    escape p c | p c = [c]
               | otherwise = '-' : show (ord c) ++ "-"
    isLegal ':' = True
    isLegal '_' = True
    isLegal '.' = True
    isLegal c = isAscii c && isAlphaNum c
       -- NB: '-' is legal in IDs, but we use it as the escape char


-------------------------------------------------------------------------------
-- * Files we need to copy from our $libdir
-------------------------------------------------------------------------------


haddockJsFile :: IsString s => s
haddockJsFile = "haddock-bundle.min.js"

jsQuickJumpFile :: IsString s => s
jsQuickJumpFile = "quick-jump.min.js"

quickJumpCssFile :: IsString s => s
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

escapeStr :: LText -> LText
escapeStr = escapeURIString isUnreserved


-- Following few functions are copy'n'pasted from Network.URI module
-- to avoid depending on the network lib, since doing so gives a
-- circular build dependency between haddock and network
-- (at least if you want to build network with haddock docs)
escapeURIChar :: (Char -> Bool) -> Char -> LText
escapeURIChar p c
    | p c       = LText.singleton c
    | otherwise = "%" <> myShowHex (ord c)
    where
        myShowHex :: Int -> LText
        myShowHex n =  case showIntAtBase 16 toChrHex n "" of
            []  -> "00"
            [a] -> "0" <> LText.singleton a
            cs  -> LText.pack cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))


escapeURIString :: (Char -> Bool) -> LText -> LText
escapeURIString p = LText.foldr (\c acc -> escapeURIChar p c <> acc) mempty


isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNumChar c || (c `elem` ("-_.~" :: String))


isAlphaChar, isDigitChar, isAlphaNumChar :: Char -> Bool
isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
isDigitChar c    = c >= '0' && c <= '9'
isAlphaNumChar c = isAlphaChar c || isDigitChar c

-- | Utility to write output to UTF-8 encoded files.
--
-- The problem with 'writeFile' is that it picks up its 'TextEncoding' from
-- 'getLocaleEncoding', and on some platforms (like Windows) this default
-- encoding isn't enough for the characters we want to write.
writeUtf8File :: FilePath -> Builder -> IO ()
writeUtf8File = Builder.writeFile

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

-----------------------------------------------------------------------------
-- * System tools
-----------------------------------------------------------------------------


#ifdef mingw32_HOST_OS
foreign import ccall unsafe "_getpid" getProcessID :: IO Int -- relies on Int == Int32 on Windows
#else
getProcessID :: IO Int
getProcessID = fmap fromIntegral System.Posix.Internals.c_getpid
#endif
