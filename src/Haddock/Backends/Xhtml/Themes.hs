-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.Themes
-- Copyright   :  (c) Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Themes (
    Themes,
    getThemes,

    cssFiles, styleSheet, stylePickers, styleMenu
    )
    where

import Haddock.Backends.Xhtml.Utils (onclick)
import Haddock.Options

import Control.Monad (liftM)
import Data.Either (lefts, rights)
import Data.List (nub)
import Data.Maybe (listToMaybe)

import System.Directory
import System.FilePath
import Text.XHtml hiding ( name, title, p, quote )
import qualified Text.XHtml as XHtml


--------------------------------------------------------------------------------
-- * CSS Themes
--------------------------------------------------------------------------------

data CssTheme = CssTheme {
  themeName :: String,
  themeHref :: String,
  themeFiles :: [FilePath]
  }

type Themes = [CssTheme]


-- | Standard theme used by default
standardTheme :: FilePath -> CssTheme
standardTheme libDir = locateIn libDir $
  CssTheme "Ocean"   "nhaddock.css" ["nhaddock.css", "hslogo-16.png"]


-- | Default themes that are part of Haddock; added with --default-themes
defaultThemes :: FilePath -> Themes
defaultThemes libDir = standardTheme libDir :
  (map (locateIn libDir) $ [
    CssTheme "Classic" "xhaddock.css" ["xhaddock.css", "haskell_icon.gif"],
    CssTheme "Tibbe"   "thaddock.css" ["thaddock.css", "haskell_icon.gif"],
    CssTheme "Snappy"  "shaddock.css" ["shaddock.css", "s_haskell_icon.gif"]
    ]
  )

locateIn :: FilePath -> CssTheme -> CssTheme
locateIn libDir t = t { themeFiles = map loc (themeFiles t) }
  where loc = combine libDir . combine "html"

--------------------------------------------------------------------------------
-- * CSS Theme Arguments
--------------------------------------------------------------------------------

-- | Process input flags for CSS Theme arguments
getThemes :: FilePath -> [Flag] -> IO (Either String Themes)
getThemes libDir flags =
  liftM (someTheme . concatEither) (mapM themeFlag flags)
  where
    themeFlag :: Flag -> IO (Either String Themes)

    themeFlag (Flag_CSS path) = (liftM . liftEither) (:[]) (theme path)

    themeFlag (Flag_Themes path) = do
      itsADirectory <- doesDirectoryExist path
      if itsADirectory
        then do
          items <- getDirectoryItems path
          themes <- mapM theme items
          case rights themes of
            [] -> errMessage "no themes found in" path
            ts -> retRight ts
        else errMessage "not a valid theme directory" path

    themeFlag (Flag_DefaultThemes) = retRight (defaultThemes libDir)
    themeFlag _ = retRight []

    theme :: FilePath -> IO (Either String CssTheme)
    theme path = do
      itsAFile <- doesFileExist path
      if itsAFile
        then singleFileTheme path
        else do
          itsADirectory <- doesDirectoryExist path
          if itsADirectory
            then directoryTheme path
            else errMessage "css path doesn't exist" path

    someTheme :: Either String Themes -> Either String Themes
    someTheme (Right []) = Right [standardTheme libDir]
    someTheme est = est

errMessage :: String -> FilePath -> IO (Either String a)
errMessage msg path = return (Left (msg ++ ": \"" ++ path ++ "\""))


retRight :: a -> IO (Either String a)
retRight = return . Right


singleFileTheme :: FilePath -> IO (Either String CssTheme)
singleFileTheme path =
  if isCssFilePath path
      then retRight $ CssTheme name file [path]
      else errMessage "file extension isn't .css" path
  where
    name = takeBaseName path
    file = takeFileName path


directoryTheme :: FilePath -> IO (Either String CssTheme)
directoryTheme path = do
  items <- getDirectoryItems path
  case filter isCssFilePath items of
    [] -> errMessage "no .css file in theme directory" path
    [cf] -> retRight $ CssTheme (takeBaseName path) (takeFileName cf) items
    _ -> errMessage "more than one .css file in theme directory" path


getDirectoryItems :: FilePath -> IO [FilePath]
getDirectoryItems path =
  getDirectoryContents path >>= return . map (combine path)


isCssFilePath :: FilePath -> Bool
isCssFilePath path = takeExtension path == ".css"


--------------------------------------------------------------------------------
-- * Style Sheet Utilities
--------------------------------------------------------------------------------

cssFiles :: Themes -> [String]
cssFiles ts = nub $ concatMap themeFiles ts


styleSheet :: Themes -> Html
styleSheet ts = toHtml $ zipWith mkLink rels ts 
  where
    rels = ("stylesheet" : repeat "alternate stylesheet")
    mkLink aRel t =
      thelink
        ! [ href (themeHref t),  rel aRel, thetype "text/css",
            XHtml.title (themeName t)
          ]
        << noHtml


stylePickers :: Themes -> [Html]
stylePickers ts = map mkPicker ts
  where
    mkPicker t =
      let js = "setActiveStyleSheet('" ++ themeHref t ++ "'); return false;" in
      anchor ! [href "#", onclick js] << themeName t


styleMenu :: Themes -> Html
styleMenu [] = noHtml
styleMenu [_] = noHtml
styleMenu ts = thediv ! [identifier "style-menu-holder"] << [
    anchor ! [ href "#", onclick js ] << "Style \9662",
    unordList (stylePickers ts) ! [ identifier "style-menu", theclass "hide" ]
  ]
  where
    js = "styleMenu(); return false;"


--------------------------------------------------------------------------------
-- * Either Utilities
--------------------------------------------------------------------------------

-- These three routines are here because Haddock does not have access to the
-- Control.Monad.Error module which supplies the Functor and Monad instances
-- for Either String.

sequenceEither :: [Either a b] -> Either a [b]
sequenceEither es = maybe (Right $ rights es) Left (listToMaybe (lefts es))

liftEither :: (b -> c) -> Either a b -> Either a c
liftEither f = either Left (Right . f)

concatEither :: [Either a [b]] -> Either a [b]
concatEither = liftEither concat . sequenceEither

