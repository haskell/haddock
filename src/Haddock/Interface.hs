--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

-- Here we build the actual module interfaces. By interface we mean the 
-- information that is used to render a Haddock page for a module. Parts of 
-- this information is also stored in the interface files.


module Haddock.Interface (
  createInterfaces
) where


import Haddock.DocName
import Haddock.Interface.Create
import Haddock.Interface.AttachInstances
import Haddock.Interface.Rename
import Haddock.Types
import Haddock.Options
import Haddock.GHC.Utils
import Haddock.GHC.Typecheck
import Haddock.Exception
import Haddock.Utils

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Control.Monad

import GHC
import Name
import HscTypes ( msHsFilePath )
import Digraph
import BasicTypes
import SrcLoc


-- | Turn a topologically sorted list of module names/filenames into interfaces. Also
-- return the home link environment created in the process, and any error messages.
#if __GLASGOW_HASKELL__ >= 609
createInterfaces :: [String] -> LinkEnv -> [Flag] -> Ghc ([Interface], LinkEnv)
createInterfaces modules externalLinks flags = do
  -- part 1, create interfaces
  interfaces <- createInterfaces' modules flags
#else
createInterfaces :: Session -> [String] -> LinkEnv -> [Flag] -> IO ([Interface], LinkEnv)
createInterfaces session modules externalLinks flags = do
  -- part 1, create interfaces
  interfaces <- createInterfaces' session modules flags
#endif
  -- part 2, build link environment
  let homeLinks = buildHomeLinks interfaces
      links     = homeLinks `Map.union` externalLinks
      allNames  = Map.keys links

  -- part 3, attach instances
  let interfaces' = attachInstances interfaces allNames

  -- part 4, rename interfaces
  let warnings = Flag_NoWarnings `notElem` flags
  let (interfaces'', msgs) = 
         runWriter $ mapM (renameInterface links warnings) interfaces'
  liftIO $ mapM_ putStrLn msgs

  return (interfaces'', homeLinks)  


#if __GLASGOW_HASKELL__ >= 609
createInterfaces' :: [String] -> [Flag] -> Ghc [Interface]
createInterfaces' modules flags = do
  targets <- mapM (\f -> guessTarget f Nothing) modules
  setTargets targets
  modgraph <- depanal [] False
  let orderedMods = flattenSCCs $ topSortModuleGraph False modgraph Nothing
  (ifaces, _) <- foldM (\(ifaces, modMap) modsum -> do
    interface <- processModule modsum flags modMap
#else
createInterfaces' :: Session -> [String] -> [Flag] -> IO [Interface]
createInterfaces' session modules flags = do
  targets <- mapM (\f -> guessTarget f Nothing) modules
  setTargets session targets
  mbGraph <- depanal session [] False
  modgraph <- case mbGraph of
    Just graph -> return graph
    Nothing -> throwE "Failed to create dependecy graph"
  let orderedMods = flattenSCCs $ topSortModuleGraph False modgraph Nothing
  (ifaces, _) <- foldM (\(ifaces, modMap) modsum -> do
    interface <- processModule session modsum flags modMap
#endif
    return $ (interface : ifaces , Map.insert (ifaceMod interface) interface modMap)
    ) ([], Map.empty) orderedMods
  return (reverse ifaces)

{-    liftIO $ do
     putStrLn . ppModInfo $ ifaceInfo interface
     putStrLn . show $ fmap pretty (ifaceDoc interface)
     print (ifaceOptions interface)
     mapM (putStrLn . pretty . fst) (Map.elems . ifaceDeclMap $ interface)
     mapM (putStrLn . show . fmap pretty . snd) (Map.elems . ifaceDeclMap $ interface)
     mapM (putStrLn . ppExportItem) (ifaceExportItems interface)
     mapM (putStrLn . pretty) (ifaceLocals interface)
     mapM (putStrLn . pretty) (ifaceExports interface)
     mapM (putStrLn . pretty) (ifaceVisibleExports interface)
     mapM (putStrLn . pretty) (ifaceInstances interface)
     mapM (\(a,b) -> putStrLn $ pretty a ++ pretty b)  (Map.toList $ ifaceSubMap interface)
     mapM (putStrLn . pretty) (ifaceInstances interface)-}

{-

ppInsts = concatMap ppInst 

ppInst (a,b,c) = concatMap pretty a ++ pretty b ++ concatMap pretty c 


ppExportItem (ExportDecl decl (Just doc) insts) = pretty decl ++ pretty doc ++ ppInsts insts
ppExportItem (ExportDecl decl Nothing insts) = pretty decl ++ ppInsts insts
ppExportItem (ExportNoDecl name name2 names) = pretty name ++ pretty name2 ++ pretty names
ppExportItem (ExportGroup level id doc) = show level ++ show id ++ pretty doc
ppExportItem (ExportDoc doc) = pretty doc
ppExportItem (ExportModule mod) = pretty mod


ppModInfo (HaddockModInfo a b c d) = show (fmap pretty a) ++ show b ++ show c ++ show d 
-}

#if __GLASGOW_HASKELL__ >= 609
processModule :: ModSummary -> [Flag] -> ModuleMap -> Ghc Interface
processModule modsum flags modMap = 

  let handleSrcErrors action = flip handleSourceError action $ \err -> do 
        printExceptionAndWarnings err
        throwE ("Failed to check module: " ++ moduleString (ms_mod modsum))

  in handleSrcErrors $ do
       let filename = msHsFilePath modsum
       let dynflags = ms_hspp_opts modsum
       tc_mod <- loadModule =<< typecheckModule =<< parseModule modsum
       let Just renamed_src = renamedSource tc_mod
       let ghcMod = mkGhcModule (ms_mod modsum,
                             filename,
                             (parsedSource tc_mod,
                              renamed_src,
                              typecheckedSource tc_mod,
                              moduleInfo tc_mod))
                             dynflags
       let (interface, msg) = runWriter $ createInterface ghcMod flags modMap
       liftIO $ mapM_ putStrLn msg
       return interface
#else
processModule :: Session -> ModSummary -> [Flag] -> ModuleMap -> IO Interface
processModule session modsum flags modMap = do
  let filename = msHsFilePath modsum
  mbMod <- checkAndLoadModule session modsum False
  ghcMod <- case mbMod of
    Just (CheckedModule a (Just b) (Just c) (Just d) _)
      -> return $ mkGhcModule (ms_mod modsum, filename, (a,b,c,d)) (ms_hspp_opts modsum)
    _ -> throwE ("Failed to check module: " ++ (moduleString $ ms_mod modsum))
  let (interface, msg) = runWriter $ createInterface ghcMod flags modMap
  mapM_ putStrLn msg
  return interface
#endif

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
        exported_names = ifaceVisibleExports iface
        mod            = ifaceMod iface
        keep_old env n = Map.insertWith (\new old -> old) n mod env
        keep_new env n = Map.insert n mod env
