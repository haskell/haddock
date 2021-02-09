{-# LANGUAGE CPP, OverloadedStrings, BangPatterns, NamedFieldPuns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2010,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module typechecks Haskell modules using the GHC API and processes
-- the result to create 'Interface's. The typechecking and the 'Interface'
-- creation is interleaved, so that when a module is processed, the
-- 'Interface's of all previously processed modules are available. The
-- creation of an 'Interface' from a typechecked module is delegated to
-- "Haddock.Interface.Create".
--
-- When all modules have been typechecked and processed, information about
-- instances are attached to each 'Interface'. This task is delegated to
-- "Haddock.Interface.AttachInstances". Note that this is done as a separate
-- step because GHC can't know about all instances until all modules have been
-- typechecked.
--
-- As a last step a link environment is built which maps names to the \"best\"
-- places to link to in the documentation, and all 'Interface's are \"renamed\"
-- using this environment.
-----------------------------------------------------------------------------
module Haddock.Interface (
    plugin
  , processModules
) where


import Haddock.GhcUtils
import Haddock.InterfaceFile
import Haddock.Interface.Create
import Haddock.Interface.AttachInstances
import Haddock.Interface.Rename
import Haddock.Options hiding (verbosity)
import Haddock.Types
import Haddock.Utils

import Control.Monad
import Control.Monad.IO.Class ( MonadIO )
import Data.IORef
import Data.List (foldl', isPrefixOf, nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf

import GHC.Unit.Module.Env (mkModuleSet, emptyModuleSet, unionModuleSet, ModuleSet)
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Graph
import GHC.Unit.Types
import GHC.Data.Graph.Directed
import GHC.Driver.Session hiding (verbosity)
import GHC hiding (verbosity)
import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Data.FastString (unpackFS)
import GHC.Utils.Error
import GHC.Tc.Types (TcM, TcGblEnv(..))
import GHC.Tc.Utils.Monad (getTopEnv, setGblEnv)
import GHC.Tc.Utils.Env (tcLookupGlobal)
import GHC.Types.Name (nameIsFromExternalPackage, nameOccName)
import GHC.Types.Name.Occurrence (isTcOcc)
import GHC.Types.Name.Reader (unQualOK, greMangledName, globalRdrEnvElts)
import GHC.HsToCore.Docs
import GHC.Plugins (Outputable, StaticPlugin(..), Plugin(..), PluginWithArgs(..),
                     defaultPlugin, keepRenamedSource)

#if defined(mingw32_HOST_OS)
import System.IO
import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
#endif

-- | Create 'Interface's and a link environment by typechecking the list of
-- modules using the GHC API and processing the resulting syntax trees.
processModules
  :: Verbosity                  -- ^ Verbosity of logging to 'stdout'
  -> [String]                   -- ^ A list of file or module names sorted by
                                -- module topology
  -> [Flag]                     -- ^ Command-line flags
  -> [InterfaceFile]            -- ^ Interface files of package dependencies
  -> Ghc ([Interface], LinkEnv) -- ^ Resulting list of interfaces and renaming
                                -- environment
processModules verbosity modules flags extIfaces = do
#if defined(mingw32_HOST_OS)
  -- Avoid internal error: <stderr>: hPutChar: invalid argument (invalid character)' non UTF-8 Windows
  liftIO $ hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
  liftIO $ hSetEncoding stderr $ mkLocaleEncoding TransliterateCodingFailure
#endif

  out verbosity verbose "Creating interfaces..."
  let
    instIfaceMap :: InstIfaceMap
    instIfaceMap = Map.fromList
      [ (instMod iface, iface)
      | ext <- extIfaces
      , iface <- ifInstalledIfaces ext
      ]

  (interfaces, ms) <- createIfaces verbosity modules flags instIfaceMap

  let exportedNames =
        Set.unions $ map (Set.fromList . ifaceExports) $
        filter (\i -> not $ OptHide `elem` ifaceOptions i) interfaces
      mods = Set.fromList $ map ifaceMod interfaces
  out verbosity verbose "Attaching instances..."
  interfaces' <- {-# SCC attachInstances #-}
                 withTimingM "attachInstances" (const ()) $ do
                   attachInstances (exportedNames, mods) interfaces instIfaceMap ms

  out verbosity verbose "Building cross-linking environment..."
  -- Combine the link envs of the external packages into one
  let extLinks  = Map.unions (map ifLinkEnv extIfaces)
      homeLinks = buildHomeLinks interfaces' -- Build the environment for the home
                                             -- package
      links     = homeLinks `Map.union` extLinks

  out verbosity verbose "Renaming interfaces..."
  let warnings = Flag_NoWarnings `notElem` flags
  dflags <- getDynFlags
  let (interfaces'', msgs) =
         runWriter $ mapM (renameInterface dflags (ignoredSymbols flags) links warnings) interfaces'
  liftIO $ mapM_ putStrLn msgs

  return (interfaces'', homeLinks)


--------------------------------------------------------------------------------
-- * Module typechecking and Interface creation
--------------------------------------------------------------------------------


createIfaces :: Verbosity -> [String] -> [Flag] -> InstIfaceMap -> Ghc ([Interface], ModuleSet)
createIfaces verbosity modules flags instIfaceMap = do
  (haddockPlugin, getIfaces, getModules) <- liftIO $ plugin
    verbosity flags instIfaceMap

  let
    installHaddockPlugin :: HscEnv -> HscEnv
    installHaddockPlugin hsc_env = hsc_env
      {
        hsc_dflags =
          gopt_set (hsc_dflags hsc_env) Opt_PluginTrustworthy
      , hsc_static_plugins =
          haddockPlugin : hsc_static_plugins hsc_env
      }

  -- Note that we would rather use withTempSession but as long as we
  -- have the separate attachInstances step we need to keep the session
  -- alive to be able to find all the instances.
  modifySession installHaddockPlugin

  targets <- mapM (\filePath -> guessTarget filePath Nothing) modules
  setTargets targets

  loadOk <- withTimingM "load" (const ()) $
    {-# SCC load #-} GHC.load LoadAllTargets

  case loadOk of
    Failed ->
      throwE "Cannot typecheck modules"
    Succeeded -> do
      modGraph <- GHC.getModuleGraph
      ifaceMap  <- liftIO getIfaces
      moduleSet <- liftIO getModules

      let
        ifaces :: [Interface]
        ifaces =
          [ Map.findWithDefault
              (error "haddock:iface")
              (ms_mod (emsModSummary ems))
              ifaceMap
          | ModuleNode ems <- flattenSCCs $ topSortModuleGraph True modGraph Nothing
          ]

      return (ifaces, moduleSet)


-- | A `Plugin` that hooks into GHC's compilation pipeline to generate Haddock
-- interfaces. Due to the plugin nature we benefit from GHC's capabilities to
-- parallelize the compilation process.
plugin
  :: MonadIO m
  => Verbosity
  -> [Flag]
  -> InstIfaceMap
  -> m
     (
       StaticPlugin -- the plugin to install with GHC
     , m IfaceMap  -- get the processed interfaces
     , m ModuleSet -- get the loaded modules
     )
plugin verbosity flags instIfaceMap = liftIO $ do
  ifaceMapRef  <- newIORef Map.empty
  moduleSetRef <- newIORef emptyModuleSet

  let
    processTypeCheckedResult :: ModSummary -> TcGblEnv -> TcM ()
    processTypeCheckedResult mod_summary tc_gbl_env
      -- Don't do anything for hs-boot modules
      | IsBoot <- isBootSummary mod_summary =
          pure ()
      | otherwise = do
          hsc_env <- getTopEnv
          ifaces <- liftIO $ readIORef ifaceMapRef
          (iface, modules) <- withTiming (hsc_logger hsc_env) (hsc_dflags hsc_env)
                                "processModule" (const ()) $
            processModule1 verbosity flags ifaces instIfaceMap hsc_env mod_summary tc_gbl_env

          liftIO $ do
            atomicModifyIORef' ifaceMapRef $ \xs ->
              (Map.insert (ms_mod mod_summary) iface xs, ())

            atomicModifyIORef' moduleSetRef $ \xs ->
              (modules `unionModuleSet` xs, ())

    staticPlugin :: StaticPlugin
    staticPlugin = StaticPlugin
      {
        spPlugin = PluginWithArgs
        {
          paPlugin = defaultPlugin
          {
            renamedResultAction = keepRenamedSource
          , typeCheckResultAction = \_ mod_summary tc_gbl_env -> setGblEnv tc_gbl_env $ do
              processTypeCheckedResult mod_summary tc_gbl_env
              pure tc_gbl_env

          }
        , paArguments = []
        }
      }

  pure
    ( staticPlugin
    , liftIO (readIORef ifaceMapRef)
    , liftIO (readIORef moduleSetRef)
    )


processModule1
  :: Verbosity
  -> [Flag]
  -> IfaceMap
  -> InstIfaceMap
  -> HscEnv
  -> ModSummary
  -> TcGblEnv
  -> TcM (Interface, ModuleSet)
processModule1 verbosity flags ifaces inst_ifaces hsc_env mod_summary tc_gbl_env = do
  out verbosity verbose "Creating interface..."

  let
    TcGblEnv { tcg_rdr_env } = tc_gbl_env

    unit_state = hsc_units hsc_env

  (!interface, messages) <- do
    logger <- getLogger
    dflags <- getDynFlags
    {-# SCC createInterface #-}
     withTiming logger dflags "createInterface" (const ()) $ runIfM (fmap Just . tcLookupGlobal) $
      createInterface1 flags unit_state mod_summary tc_gbl_env
        ifaces inst_ifaces

  -- We need to keep track of which modules were somehow in scope so that when
  -- Haddock later looks for instances, it also looks in these modules too.
  --
  -- See https://github.com/haskell/haddock/issues/469.
  let
    mods :: ModuleSet
    !mods = mkModuleSet
      [ nameModule name
      | gre <- globalRdrEnvElts tcg_rdr_env
      , let name = greMangledName gre
      , nameIsFromExternalPackage (hsc_home_unit hsc_env) name
      , isTcOcc (nameOccName name)   -- Types and classes only
      , unQualOK gre -- In scope unqualified
      ]

  liftIO $ mapM_ putStrLn (nub messages)
  dflags <- getDynFlags

  let
    (haddockable, haddocked) =
      ifaceHaddockCoverage interface

    percentage :: Int
    percentage = div (haddocked * 100) haddockable

    modString :: String
    modString = moduleString (ifaceMod interface)

    coverageMsg :: String
    coverageMsg =
      printf " %3d%% (%3d /%3d) in '%s'" percentage haddocked haddockable modString

    header :: Bool
    header = case ifaceDoc interface of
      Documentation Nothing _ -> False
      _ -> True

    undocumentedExports :: [String]
    undocumentedExports =
      [ formatName s n
      | ExportDecl { expItemDecl = L s n
                   , expItemMbDoc = (Documentation Nothing _, _)
                   } <- ifaceExportItems interface
      ]
        where
          formatName :: SrcSpan -> HsDecl GhcRn -> String
          formatName loc n = p (getMainDeclBinder n) ++ case loc of
            RealSrcSpan rss _ -> " (" ++ unpackFS (srcSpanFile rss) ++ ":" ++
              show (srcSpanStartLine rss) ++ ")"
            _ -> ""

          p :: Outputable a => [a] -> String
          p [] = ""
          p (x:_) = let n = pretty dflags x
                        ms = modString ++ "."
                    in if ms `isPrefixOf` n
                       then drop (length ms) n
                       else n

  when (OptHide `notElem` ifaceOptions interface) $ do
    out verbosity normal coverageMsg
    when (Flag_NoPrintMissingDocs `notElem` flags
          && not (null undocumentedExports && header)) $ do
      out verbosity normal "  Missing documentation for:"
      unless header $ out verbosity normal "    Module header"
      mapM_ (out verbosity normal . ("    " ++)) undocumentedExports

  pure (interface, mods)


--------------------------------------------------------------------------------
-- * Building of cross-linking environment
--------------------------------------------------------------------------------


-- | Build a mapping which for each original name, points to the "best"
-- place to link to in the documentation.  For the definition of
-- "best", we use "the module nearest the bottom of the dependency
-- graph which exports this name", not including hidden modules.  When
-- there are multiple choices, we pick a random one.
--
-- The interfaces are passed in in topologically sorted order, but we start
-- by reversing the list so we can do a foldl.
buildHomeLinks :: [Interface] -> LinkEnv
buildHomeLinks ifaces = foldl upd Map.empty (reverse ifaces)
  where
    upd old_env iface
      | OptHide    `elem` ifaceOptions iface = old_env
      | OptNotHome `elem` ifaceOptions iface =
        foldl' keep_old old_env exported_names
      | otherwise = foldl' keep_new old_env exported_names
      where
        exported_names = ifaceVisibleExports iface ++ map getName (ifaceInstances iface)
        mdl            = ifaceMod iface
        keep_old env n = Map.insertWith (\_ old -> old) n mdl env
        keep_new env n = Map.insert n mdl env
