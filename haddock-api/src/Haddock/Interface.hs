{-# LANGUAGE CPP, OverloadedStrings #-}
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
  processModules
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
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Distribution.Verbosity
import System.Directory
import System.FilePath
import Text.Printf

import Digraph
import DynFlags hiding (verbosity)
import Exception
import GHC hiding (verbosity)
import GhcMake
import HscTypes
import FastString (unpackFS)
import Module
import MonadUtils (liftIO)
import TcRnMonad
import TcRnTypes (tcg_rdr_env, Env)
import RdrName (plusGlobalRdrEnv)
import ErrUtils (withTiming, MsgDoc)
import ExtractDocs
import Outputable
import LoadIface
import MkIface
import IOEnv
import Maybes
import GhcMonad
import Packages
import Panic

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
  liftIO $ hSetEncoding stderr $ mkLocaleEncoding TransliterateCodingFailure
#endif

  out verbosity verbose "Creating interfaces..."
  let instIfaceMap =  Map.fromList [ (instMod iface, iface) | ext <- extIfaces
                                   , iface <- ifInstalledIfaces ext ]
  interfaces <- createIfaces0 verbosity modules flags instIfaceMap

  let exportedNames =
        Set.unions $ map (Set.fromList . ifaceExports) $
        filter (\i -> not $ OptHide `elem` ifaceOptions i) interfaces
      mods = Set.fromList $ map ifaceMod interfaces
  out verbosity verbose "Attaching instances..."
  interfaces' <- {-# SCC attachInstances #-}
                 withTiming getDynFlags "attachInstances" (const ()) $ do
                   attachInstances (exportedNames, mods) interfaces instIfaceMap

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
         runWriter $ mapM (renameInterface dflags links warnings) interfaces'
  liftIO $ mapM_ putStrLn msgs

  return (interfaces'', homeLinks)


--------------------------------------------------------------------------------
-- * Module typechecking and Interface creation
--------------------------------------------------------------------------------


createIfaces0 :: Verbosity -> [String] -> [Flag] -> InstIfaceMap -> Ghc [Interface]
createIfaces0 verbosity modules flags instIfaceMap =
  -- Output dir needs to be set before calling depanal since depanal uses it to
  -- compute output file names that are stored in the DynFlags of the
  -- resulting ModSummaries.
  (if useTempDir then withTempOutputDir else id) $ do
    modGraph <- depAnalysis
    createIfaces verbosity flags instIfaceMap modGraph

  where
    useTempDir :: Bool
    useTempDir = Flag_NoTmpCompDir `notElem` flags


    withTempOutputDir :: Ghc a -> Ghc a
    withTempOutputDir action = do
      tmp <- liftIO getTemporaryDirectory
      x   <- liftIO getProcessID
      let dir = tmp </> ".haddock-" ++ show x
      modifySessionDynFlags (setOutputDir dir)
      withTempDir dir action


    depAnalysis :: Ghc ModuleGraph
    depAnalysis = do
      targets <- mapM (\f -> guessTarget f Nothing) modules
      setTargets targets
      depanal [] False


createIfaces :: Verbosity -> [Flag] -> InstIfaceMap -> ModuleGraph -> Ghc [Interface]
createIfaces verbosity flags instIfaceMap mods = do
  dflags <- getDynFlags
  let sortedMods = flattenSCCs $ topSortModuleGraph False mods Nothing
  liftIO $ print $ map (showPpr dflags . ms_mod) sortedMods
  -- TODO: 
  -- * Handle failure
  -- * Use -fno-code?!
  _ <- load' LoadAllTargets Nothing mods
  out verbosity normal "Haddock coverage:"
  (ifaces, _) <- foldM f ([], Map.empty) sortedMods
  return (reverse ifaces)
  where
    f (ifaces, ifaceMap) modSummary = do
      x <- {-# SCC processModule #-} do
           dflags <- getDynFlags
           liftIO $ putStrLn $ showPpr dflags (ms_mod modSummary)
           withTiming getDynFlags "processModule" (const ()) $ do
             processModule verbosity modSummary flags ifaceMap instIfaceMap
      return $ case x of
        Just iface -> (iface:ifaces, Map.insert (ifaceMod iface) iface ifaceMap)
        Nothing    -> (ifaces, ifaceMap) -- Boot modules don't generate ifaces.


processModule :: Verbosity -> ModSummary -> [Flag] -> IfaceMap -> InstIfaceMap -> Ghc (Maybe Interface)
processModule verbosity modsum flags modMap instIfaceMap = do
  dflags <- getDynFlags
  out verbosity verbose $ "Checking module " ++ moduleString (ms_mod modsum) ++ "..."

  {-
  mbe_mod_iface <- withSession $ \hsc_env -> do
    liftIO $ initIfaceCheck (text "processModule 0") hsc_env $ do
      -- _ <- loadModuleInterface (text "processModule") (ms_mod modsum)
      findAndReadIface (text "processModule")
                       (fst (splitModuleInsts (ms_mod modsum)))
                       (ms_mod modsum)
                       False
  (mod_iface, _fp) <- case mbe_mod_iface of
    Maybes.Failed e -> throwGhcException (CmdLineError (showSDoc dflags e))
    Maybes.Succeeded r -> return r
  -}

  {-
(13:18:41) sjakobi: mpickering: Even with larger libraries, the load order seems fine. I've now found the Note [Home module load error], but don't really understand it yet. I also don't understand the difference between loadUserInterface and loadSysInterface so far.
(13:20:56) mpickering: are you updating `hsc_env` after loading interfaces?
(13:21:24) mpickering: Where does GHC do that?
(13:25:39) sjakobi: haddock already fails to load the very first interface.
(13:25:39) mpickering: I would look for places where GHC populates the place which fails in the lookup
(13:25:44) mpickering: and add traces
(13:25:55) mpickering: add some traces to GHC
(13:26:01) mpickering: then compile with haddock by the API
(13:26:06) mpickering: and directly and see what is different
(13:26:16) mpickering: You could be initialising DynFlags wrong?
(13:27:29) sjakobi: Thx mpickering. These sound like good ideas.
-}
  mod_iface <- withSession $ \hsc_env -> do
    liftIO $ initIfaceCheck (text "processModule 0") hsc_env $ do
      loadSysInterface (text "processModule 1")
                       (ms_mod modsum)


  {-
  tm <- {-# SCC "parse/typecheck/load" #-} loadModule =<< typecheckModule =<< parseModule modsum

  -- We need to modify the interactive context's environment so that when
  -- Haddock later looks for instances, it also looks in the modules it
  -- encountered while typechecking.
  --
  -- See https://github.com/haskell/haddock/issues/469.
  hsc_env@HscEnv{ hsc_IC = old_IC } <- getSession
  let new_rdr_env = tcg_rdr_env . fst . GHC.tm_internals_ $ tm
  setSession hsc_env{ hsc_IC = old_IC {
    ic_rn_gbl_env = ic_rn_gbl_env old_IC `plusGlobalRdrEnv` new_rdr_env
  } }
  -}

  {-
  dm <- desugarModule tm
  hsc_env' <- getSession
  let mod_guts = dm_core_module dm
  let mod_details = snd (tm_internals_ tm)
  (iface, _bl) <- liftIO $ mkIface hsc_env' Nothing mod_details mod_guts
  -} 
  -- liftIO $ putStrLn $ (showSDoc dflags . pprModIface) iface

  if not $ isBootSummary modsum then do
    out verbosity verbose "Creating interface..."
    (interface, msgs) <- {-# SCC createIterface #-}
                        withTiming getDynFlags "createInterface" (const ()) $ do
                          --runWriterGhc $ createInterface tm flags modMap instIfaceMap
                          runWriterGhc $ createInterface' mod_iface flags modMap instIfaceMap
    liftIO $ mapM_ putStrLn (nub msgs)
    let (haddockable, haddocked) = ifaceHaddockCoverage interface
        percentage = round (fromIntegral haddocked * 100 / fromIntegral haddockable :: Double) :: Int
        modString = moduleString (ifaceMod interface)
        coverageMsg = printf " %3d%% (%3d /%3d) in '%s'" percentage haddocked haddockable modString
        header = case ifaceDoc interface of
          Documentation Nothing _ -> False
          _ -> True
        undocumentedExports = [ formatName s n | ExportDecl { expItemDecl = L s n
                                                            , expItemMbDoc = (Documentation Nothing _, _)
                                                            } <- ifaceExportItems interface ]
          where
            formatName :: SrcSpan -> HsDecl GhcRn -> String
            formatName loc n = p (getMainDeclBinder n) ++ case loc of
              RealSrcSpan rss -> " (" ++ unpackFS (srcSpanFile rss) ++ ":" ++ show (srcSpanStartLine rss) ++ ")"
              _ -> ""

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
    interface' <- liftIO $ evaluate interface
    return (Just interface')
  else
    return Nothing


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


--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------


withTempDir :: (ExceptionMonad m) => FilePath -> m a -> m a
withTempDir dir = gbracket_ (liftIO $ createDirectory dir)
                            (liftIO $ removeDirectoryRecursive dir)
