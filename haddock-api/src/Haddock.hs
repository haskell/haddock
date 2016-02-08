{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE CPP, ScopedTypeVariables, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2010,
--                    Mateusz Kowalczyk 2014
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Haddock - A Haskell Documentation Tool
--
-- Program entry point and top-level code.
-----------------------------------------------------------------------------
module Haddock (
  haddock,
  haddockWithGhc,
  getGhcDirs,
  readPackagesAndProcessModules,
  withGhc
) where

import Data.Version
import Haddock.Backends.Xhtml
import Haddock.Backends.Xhtml.Themes (getThemes)
import Haddock.Backends.LaTeX
import Haddock.Backends.Hoogle
import Haddock.Backends.Hyperlinker
import Haddock.Interface
import Haddock.Parser
import Haddock.Types
import Haddock.Version
import Haddock.InterfaceFile
import Haddock.Options
import Haddock.Utils

import Control.Monad hiding (forM_)
import Control.Applicative
import Data.Foldable (forM_)
import Data.List (isPrefixOf)
import Control.Exception
import Data.Maybe
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import System.Exit

#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C
import Data.Int
#endif

#ifdef IN_GHC_TREE
import System.FilePath
#else
import qualified GHC.Paths as GhcPaths
import Paths_haddock_api (getDataDir)
import System.Directory (doesDirectoryExist)
#endif

import GHC hiding (verbosity)
import Config
import DynFlags hiding (projectVersion, verbosity)
import StaticFlags (discardStaticFlags)
import Packages
import Panic (handleGhcException)
import Module
import FastString

--------------------------------------------------------------------------------
-- * Exception handling
--------------------------------------------------------------------------------


handleTopExceptions :: IO a -> IO a
handleTopExceptions =
  handleNormalExceptions . handleHaddockExceptions . handleGhcExceptions


-- | Either returns normally or throws an ExitCode exception;
-- all other exceptions are turned into exit exceptions.
handleNormalExceptions :: IO a -> IO a
handleNormalExceptions inner =
  (inner `onException` hFlush stdout)
  `catches`
  [  Handler (\(code :: ExitCode) -> exitWith code)

  ,  Handler (\(ex :: AsyncException) ->
       case ex of
         StackOverflow -> do
           putStrLn "stack overflow: use -g +RTS -K<size> to increase it"
           exitFailure
         _ -> do
           putStrLn ("haddock: " ++ show ex)
           exitFailure)

  ,  Handler (\(ex :: SomeException) -> do
        putStrLn ("haddock: internal error: " ++ show ex)
        exitFailure)
  ]


handleHaddockExceptions :: IO a -> IO a
handleHaddockExceptions inner =
  catches inner [Handler handler]
  where
    handler (e::HaddockException) = do
      putStrLn $ "haddock: " ++ show e
      exitFailure


handleGhcExceptions :: IO a -> IO a
handleGhcExceptions =
  -- error messages propagated as exceptions
  handleGhcException $ \e -> do
    hFlush stdout
    print (e :: GhcException)
    exitFailure


-------------------------------------------------------------------------------
-- * Top level
-------------------------------------------------------------------------------


-- | Run Haddock with given list of arguments.
--
-- Haddock's own main function is defined in terms of this:
--
-- > main = getArgs >>= haddock
haddock :: [String] -> IO ()
haddock args = haddockWithGhc withGhc args

haddockWithGhc :: (forall a. [Flag] -> Ghc a -> IO a) -> [String] -> IO ()
haddockWithGhc ghc args = handleTopExceptions $ do

  -- Parse command-line flags and handle some of them initially.
  -- TODO: unify all of this (and some of what's in the 'render' function),
  -- into one function that returns a record with a field for each option,
  -- or which exits with an error or help message.
  (flags, files) <- parseHaddockOpts args
  shortcutFlags flags
  qual <- case qualification flags of {Left msg -> throwE msg; Right q -> return q}

  -- inject dynamic-too into flags before we proceed
  flags' <- ghc flags $ do
        df <- getDynFlags
        case lookup "GHC Dynamic" (compilerInfo df) of
          Just "YES" -> return $ Flag_OptGhc "-dynamic-too" : flags
          _ -> return flags

  unless (Flag_NoWarnings `elem` flags) $ do
    hypSrcWarnings flags
    forM_ (warnings args) $ \warning -> do
      hPutStrLn stderr warning

  ghc flags' $ do

    dflags <- getDynFlags

    if not (null files) then do
      (packages, ifaces, homeLinks) <- readPackagesAndProcessModules flags files

      -- Dump an "interface file" (.haddock file), if requested.
      forM_ (optDumpInterfaceFile flags) $ \path -> liftIO $ do
        writeInterfaceFile path InterfaceFile {
            ifInstalledIfaces = map toInstalledIface ifaces
          , ifLinkEnv         = homeLinks
          }

      -- Render the interfaces.
      liftIO $ renderStep dflags flags qual packages ifaces

    else do
      when (any (`elem` [Flag_Html, Flag_Hoogle, Flag_LaTeX]) flags) $
        throwE "No input file(s)."

      -- Get packages supplied with --read-interface.
      packages <- liftIO $ readInterfaceFiles freshNameCache (readIfaceArgs flags)

      -- Render even though there are no input files (usually contents/index).
      liftIO $ renderStep dflags flags qual packages []

-- | Create warnings about potential misuse of -optghc
warnings :: [String] -> [String]
warnings = map format . filter (isPrefixOf "-optghc")
  where
    format arg = concat ["Warning: `", arg, "' means `-o ", drop 2 arg, "', did you mean `-", arg, "'?"]


withGhc :: [Flag] -> Ghc a -> IO a
withGhc flags action = do
  libDir <- fmap snd (getGhcDirs flags)

  -- Catches all GHC source errors, then prints and re-throws them.
  let handleSrcErrors action' = flip handleSourceError action' $ \err -> do
        printException err
        liftIO exitFailure

  withGhc' libDir (ghcFlags flags) (\_ -> handleSrcErrors action)


readPackagesAndProcessModules :: [Flag] -> [String]
                              -> Ghc ([(DocPaths, InterfaceFile)], [Interface], LinkEnv)
readPackagesAndProcessModules flags files = do
    -- Get packages supplied with --read-interface.
    packages <- readInterfaceFiles nameCacheFromGhc (readIfaceArgs flags)

    -- Create the interfaces -- this is the core part of Haddock.
    let ifaceFiles = map snd packages
    (ifaces, homeLinks) <- processModules (verbosity flags) files flags ifaceFiles

    return (packages, ifaces, homeLinks)


renderStep :: DynFlags -> [Flag] -> QualOption -> [(DocPaths, InterfaceFile)] -> [Interface] -> IO ()
renderStep dflags flags qual pkgs interfaces = do
  updateHTMLXRefs pkgs
  let
    ifaceFiles = map snd pkgs
    installedIfaces = concatMap ifInstalledIfaces ifaceFiles
    extSrcMap = Map.fromList $ do
      ((_, Just path), ifile) <- pkgs
      iface <- ifInstalledIfaces ifile
      return (instMod iface, path)
  render dflags flags qual interfaces installedIfaces extSrcMap


-- | Render the interfaces with whatever backend is specified in the flags.
render :: DynFlags -> [Flag] -> QualOption -> [Interface] -> [InstalledInterface] -> Map Module FilePath -> IO ()
render dflags flags qual ifaces installedIfaces extSrcMap = do

  let
    title                = fromMaybe "" (optTitle flags)
    unicode              = Flag_UseUnicode `elem` flags
    pretty               = Flag_PrettyHtml `elem` flags
    opt_wiki_urls        = wikiUrls          flags
    opt_contents_url     = optContentsUrl    flags
    opt_index_url        = optIndexUrl       flags
    odir                 = outputDir         flags
    opt_latex_style      = optLaTeXStyle     flags
    opt_source_css       = optSourceCssFile  flags
    opt_mathjax          = optMathjax        flags
    dflags'
      | unicode          = gopt_set dflags Opt_PrintUnicodeSyntax
      | otherwise        = dflags

    visibleIfaces    = [ i | i <- ifaces, OptHide `notElem` ifaceOptions i ]

    -- /All/ visible interfaces including external package modules.
    allIfaces        = map toInstalledIface ifaces ++ installedIfaces
    allVisibleIfaces = [ i | i <- allIfaces, OptHide `notElem` instOptions i ]

    pkgMod           = ifaceMod (head ifaces)
    pkgKey           = moduleUnitId pkgMod
    pkgStr           = Just (unitIdString pkgKey)
    pkgNameVer       = modulePackageInfo dflags flags pkgMod

    (srcBase, srcModule, srcEntity, srcLEntity) = sourceUrls flags

    srcModule'
      | Flag_HyperlinkedSource `elem` flags = Just hypSrcModuleUrlFormat
      | otherwise = srcModule

    srcMap = mkSrcMap $ Map.union
      (Map.map SrcExternal extSrcMap)
      (Map.fromList [ (ifaceMod iface, SrcLocal) | iface <- ifaces ])

    pkgSrcMap = Map.mapKeys moduleUnitId extSrcMap
    pkgSrcMap'
      | Flag_HyperlinkedSource `elem` flags =
          Map.insert pkgKey hypSrcModuleNameUrlFormat pkgSrcMap
      | Just srcNameUrl <- srcEntity = Map.insert pkgKey srcNameUrl pkgSrcMap
      | otherwise = pkgSrcMap

    -- TODO: Get these from the interface files as with srcMap
    pkgSrcLMap'
      | Flag_HyperlinkedSource `elem` flags =
          Map.singleton pkgKey hypSrcModuleLineUrlFormat
      | Just path <- srcLEntity = Map.singleton pkgKey path
      | otherwise = Map.empty

    sourceUrls' = (srcBase, srcModule', pkgSrcMap', pkgSrcLMap')

  libDir   <- getHaddockLibDir flags
  prologue <- getPrologue dflags' flags
  themes   <- getThemes libDir flags >>= either bye return

  when (Flag_GenIndex `elem` flags) $ do
    ppHtmlIndex odir title pkgStr
                themes opt_mathjax opt_contents_url sourceUrls' opt_wiki_urls
                allVisibleIfaces pretty
    copyHtmlBits odir libDir themes

  when (Flag_GenContents `elem` flags) $ do
    ppHtmlContents dflags' odir title pkgStr
                   themes opt_mathjax opt_index_url sourceUrls' opt_wiki_urls
                   allVisibleIfaces True prologue pretty
                   (makeContentsQual qual)
    copyHtmlBits odir libDir themes

  when (Flag_Html `elem` flags) $ do
    ppHtml dflags' title pkgStr visibleIfaces odir
                prologue
                themes opt_mathjax sourceUrls' opt_wiki_urls
                opt_contents_url opt_index_url unicode qual
                pretty
    copyHtmlBits odir libDir themes

  -- TODO: we throw away Meta for both Hoogle and LaTeX right now,
  -- might want to fix that if/when these two get some work on them
  when (Flag_Hoogle `elem` flags) $ do
    case pkgNameVer of
      Nothing -> putStrLn . unlines $
          [ "haddock: Unable to find a package providing module "
            ++ moduleNameString (moduleName pkgMod) ++ ", skipping Hoogle."
          , ""
          , "         Perhaps try specifying the desired package explicitly"
            ++ " using the --package-name"
          , "         and --package-version arguments."
          ]
      Just (PackageName pkgNameFS, pkgVer) ->
          let pkgNameStr | unpackFS pkgNameFS == "main" && title /= [] = title
                         | otherwise = unpackFS pkgNameFS
          in ppHoogle dflags' pkgNameStr pkgVer title (fmap _doc prologue)
               visibleIfaces odir

  when (Flag_LaTeX `elem` flags) $ do
    ppLaTeX title pkgStr visibleIfaces odir (fmap _doc prologue) opt_latex_style
                  libDir

  when (Flag_HyperlinkedSource `elem` flags) $ do
    ppHyperlinkedSource odir libDir opt_source_css pretty srcMap ifaces

-- | From GHC 7.10, this function has a potential to crash with a
-- nasty message such as @expectJust getPackageDetails@ because
-- package name and versions can no longer reliably be extracted in
-- all cases: if the package is not installed yet then this info is no
-- longer available. The @--package-name@ and @--package-version@
-- Haddock flags allow the user to specify this information and it is
-- returned here if present: if it is not present, the error will
-- occur. Nasty but that's how it is for now. Potential TODO.
modulePackageInfo :: DynFlags
                  -> [Flag] -- ^ Haddock flags are checked as they may
                            -- contain the package name or version
                            -- provided by the user which we
                            -- prioritise
                  -> Module -> Maybe (PackageName, Data.Version.Version)
modulePackageInfo dflags flags modu =
    cmdline <|> pkgDb
  where
    cmdline = (,) <$> optPackageName flags <*> optPackageVersion flags
    pkgDb = (\pkg -> (packageName pkg, packageVersion pkg)) <$> lookupPackage dflags (moduleUnitId modu)


-------------------------------------------------------------------------------
-- * Reading and dumping interface files
-------------------------------------------------------------------------------


readInterfaceFiles :: MonadIO m
                   => NameCacheAccessor m
                   -> [(DocPaths, FilePath)]
                   -> m [(DocPaths, InterfaceFile)]
readInterfaceFiles name_cache_accessor pairs = do
  catMaybes `liftM` mapM tryReadIface pairs
  where
    -- try to read an interface, warn if we can't
    tryReadIface (paths, file) =
      readInterfaceFile name_cache_accessor file >>= \case
        Left err -> liftIO $ do
          putStrLn ("Warning: Cannot read " ++ file ++ ":")
          putStrLn ("   " ++ err)
          putStrLn "Skipping this interface."
          return Nothing
        Right f -> return $ Just (paths, f)


-------------------------------------------------------------------------------
-- * Creating a GHC session
-------------------------------------------------------------------------------


-- | Start a GHC session with the -haddock flag set. Also turn off
-- compilation and linking. Then run the given 'Ghc' action.
withGhc' :: String -> [String] -> (DynFlags -> Ghc a) -> IO a
withGhc' libDir flags ghcActs = runGhc (Just libDir) $ do
  dynflags  <- getSessionDynFlags
  dynflags' <- parseGhcFlags (gopt_set dynflags Opt_Haddock) {
    hscTarget = HscNothing,
    ghcMode   = CompManager,
    ghcLink   = NoLink
    }
  let dynflags'' = gopt_unset dynflags' Opt_SplitObjs
  defaultCleanupHandler dynflags'' $ do
      -- ignore the following return-value, which is a list of packages
      -- that may need to be re-linked: Haddock doesn't do any
      -- dynamic or static linking at all!
      _ <- setSessionDynFlags dynflags''
      ghcActs dynflags''
  where
    parseGhcFlags :: MonadIO m => DynFlags -> m DynFlags
    parseGhcFlags dynflags = do
      -- TODO: handle warnings?

      -- NOTA BENE: We _MUST_ discard any static flags here, because we cannot
      -- rely on Haddock to parse them, as it only parses the DynFlags. Yet if
      -- we pass any, Haddock will fail. Since StaticFlags are global to the
      -- GHC invocation, there's also no way to reparse/save them to set them
      -- again properly.
      --
      -- This is a bit of a hack until we get rid of the rest of the remaining
      -- StaticFlags. See GHC issue #8276.
      let flags' = discardStaticFlags flags
      (dynflags', rest, _) <- parseDynamicFlags dynflags (map noLoc flags')
      if not (null rest)
        then throwE ("Couldn't parse GHC options: " ++ unwords flags')
        else return dynflags'

-------------------------------------------------------------------------------
-- * Misc
-------------------------------------------------------------------------------


getHaddockLibDir :: [Flag] -> IO String
getHaddockLibDir flags =
  case [str | Flag_Lib str <- flags] of
    [] -> do
#ifdef IN_GHC_TREE
      getInTreeDir
#else
      d <- getDataDir -- provided by Cabal
      doesDirectoryExist d >>= \exists -> case exists of
        True -> return d
        False -> do
          -- If directory does not exist then we are probably invoking from
          -- ./dist/build/haddock/haddock so we use ./resources as a fallback.
          doesDirectoryExist "resources" >>= \exists_ -> case exists_ of
            True -> return "resources"
            False -> die ("Haddock's resource directory (" ++ d ++ ") does not exist!\n")
#endif
    fs -> return (last fs)


getGhcDirs :: [Flag] -> IO (String, String)
getGhcDirs flags = do
  case [ dir | Flag_GhcLibDir dir <- flags ] of
    [] -> do
#ifdef IN_GHC_TREE
      libDir <- getInTreeDir
      return (ghcPath, libDir)
#else
      return (ghcPath, GhcPaths.libdir)
#endif
    xs -> return (ghcPath, last xs)
  where
#ifdef IN_GHC_TREE
    ghcPath = "not available"
#else
    ghcPath = GhcPaths.ghc
#endif


shortcutFlags :: [Flag] -> IO ()
shortcutFlags flags = do
  usage <- getUsage

  when (Flag_Help             `elem` flags) (bye usage)
  when (Flag_Version          `elem` flags) byeVersion
  when (Flag_InterfaceVersion `elem` flags) (bye (show binaryInterfaceVersion ++ "\n"))
  when (Flag_CompatibleInterfaceVersions `elem` flags)
    (bye (unwords (map show binaryInterfaceVersionCompatibility) ++ "\n"))
  when (Flag_GhcVersion       `elem` flags) (bye (cProjectVersion ++ "\n"))

  when (Flag_PrintGhcPath `elem` flags) $ do
    dir <- fmap fst (getGhcDirs flags)
    bye $ dir ++ "\n"

  when (Flag_PrintGhcLibDir `elem` flags) $ do
    dir <- fmap snd (getGhcDirs flags)
    bye $ dir ++ "\n"

  when (Flag_UseUnicode `elem` flags && Flag_Html `notElem` flags) $
    throwE "Unicode can only be enabled for HTML output."

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
        && Flag_Html `elem` flags) $
    throwE "-h cannot be used with --gen-index or --gen-contents"

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
        && Flag_Hoogle `elem` flags) $
    throwE "--hoogle cannot be used with --gen-index or --gen-contents"

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
        && Flag_LaTeX `elem` flags) $
    throwE "--latex cannot be used with --gen-index or --gen-contents"
  where
    byeVersion = bye $
      "Haddock version " ++ projectVersion ++ ", (c) Simon Marlow 2006\n"
      ++ "Ported to use the GHC API by David Waern 2006-2008\n"


-- | Generate some warnings about potential misuse of @--hyperlinked-source@.
hypSrcWarnings :: [Flag] -> IO ()
hypSrcWarnings flags = do

    when (hypSrc && any isSourceUrlFlag flags) $
        hPutStrLn stderr $ concat
            [ "Warning: "
            , "--source-* options are ignored when "
            , "--hyperlinked-source is enabled."
            ]

    when (not hypSrc && any isSourceCssFlag flags) $
        hPutStrLn stderr $ concat
            [ "Warning: "
            , "source CSS file is specified but "
            , "--hyperlinked-source is disabled."
            ]

  where
    hypSrc = Flag_HyperlinkedSource `elem` flags
    isSourceUrlFlag (Flag_SourceBaseURL _) = True
    isSourceUrlFlag (Flag_SourceModuleURL _) = True
    isSourceUrlFlag (Flag_SourceEntityURL _) = True
    isSourceUrlFlag (Flag_SourceLEntityURL _) = True
    isSourceUrlFlag _ = False
    isSourceCssFlag (Flag_SourceCss _) = True
    isSourceCssFlag _ = False


updateHTMLXRefs :: [(DocPaths, InterfaceFile)] -> IO ()
updateHTMLXRefs packages = do
  writeIORef html_xrefs_ref (Map.fromList mapping)
  writeIORef html_xrefs_ref' (Map.fromList mapping')
  where
    mapping = [ (instMod iface, html) | ((html, _), ifaces) <- packages
              , iface <- ifInstalledIfaces ifaces ]
    mapping' = [ (moduleName m, html) | (m, html) <- mapping ]


getPrologue :: DynFlags -> [Flag] -> IO (Maybe (MDoc RdrName))
getPrologue dflags flags =
  case [filename | Flag_Prologue filename <- flags ] of
    [] -> return Nothing
    [filename] -> withFile filename ReadMode $ \h -> do
      hSetEncoding h utf8
      str <- hGetContents h
      return . Just $! parseParas dflags str
    _ -> throwE "multiple -p/--prologue options"


#ifdef IN_GHC_TREE

getInTreeDir :: IO String
getInTreeDir = getExecDir >>= \case
  Nothing -> error "No GhcDir found"
  Just d -> return (d </> ".." </> "lib")


getExecDir :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
getExecDir = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return Nothing
          _ | ret < size -> fmap (Just . dropFileName) $ peekCWString buf
            | otherwise  -> try_size (size * 2)

foreign import stdcall unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32
#else
getExecDir = return Nothing
#endif

#endif
