--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.InterfaceFile (
  InterfaceFile(..),
  writeInterfaceFile,
  readInterfaceFile,
  iface2interface
) where


import Haddock.Types
import Haddock.Exception

import Data.List
import Data.Word
import Data.Array
import Data.IORef
import qualified Data.Map as Map
import System.IO
import Control.Monad

import GHC
import SrcLoc   ( noSrcSpan ) -- tmp, GHC now exports this
import Binary
import Name
import UniqSupply
import UniqFM
import IfaceEnv
import Module
import Packages
import HscTypes
import FastMutInt
import InstEnv
import HsDoc

data InterfaceMod = InterfaceMod {
  imModule       :: Module,
  imFilename     :: FilePath,
  imExportItems  :: [ExportItem DocName]
}

data InterfaceFile = InterfaceFile {
  ifLinkEnv :: LinkEnv,
  ifModules :: [Module]  
} 

instance Binary InterfaceFile where
  put_ bh (InterfaceFile env mods) = do
    put_ bh (Map.toList env)
    put_ bh mods

  get bh = do
    env  <- get bh
    mods <- get bh
    return (InterfaceFile (Map.fromList env) mods)

iface2interface iface = InterfaceMod {
  imModule      = ifaceMod             iface,
  imFilename    = ifaceOrigFilename   iface,
  imExportItems = ifaceRnExportItems iface
}
  
binaryInterfaceMagic = 0xD0Cface :: Word32
binaryInterfaceVersion = 0 :: Word16

initBinMemSize = (1024*1024) :: Int

writeInterfaceFile :: FilePath -> InterfaceFile -> IO ()
writeInterfaceFile filename iface = do 
  bh <- openBinMem initBinMemSize
  put_ bh binaryInterfaceMagic
  put_ bh binaryInterfaceVersion

  -- remember where the dictionary pointer will go
  dict_p_p <- tellBin bh
  put_ bh dict_p_p	

  -- remember where the symbol table pointer will go
  symtab_p_p <- tellBin bh
  put_ bh symtab_p_p

  -- Make some intial state
  ud <- newWriteState

  -- put the main thing
  bh <- return $ setUserData bh ud
  put_ bh iface

  -- write the symtab pointer at the fornt of the file
  symtab_p <- tellBin bh
  putAt bh symtab_p_p symtab_p
  seekBin bh symtab_p		

  -- write the symbol table itself
  symtab_next <- readFastMutInt (ud_symtab_next ud)
  symtab_map  <- readIORef (ud_symtab_map ud)
  putSymbolTable bh symtab_next symtab_map

  -- write the dictionary pointer at the fornt of the file
  dict_p <- tellBin bh
  putAt bh dict_p_p dict_p
  seekBin bh dict_p

  -- write the dictionary itself
  dict_next <- readFastMutInt (ud_dict_next ud)
  dict_map  <- readIORef (ud_dict_map ud)
  putDictionary bh dict_next dict_map

	-- snd send the result to the file
  writeBinMem bh filename
  return ()


readInterfaceFile :: Session -> FilePath -> IO (Either String InterfaceFile)
readInterfaceFile session filename = do
  bh <- readBinMem filename

  magic   <- get bh
  version <- get bh

  case () of
    _ | magic /= binaryInterfaceMagic -> return . Left $
      "Magic number mismatch: couldn't load interface file: " ++ filename
      | version /= binaryInterfaceVersion -> return . Left $
      "Interface file is of wrong version: " ++ filename
      | otherwise -> do

      -- get the dictionary
      dict_p <- get bh
      data_p <- tellBin bh		
      seekBin bh dict_p
      dict <- getDictionary bh
      seekBin bh data_p		

      -- initialise the user-data field of bh
      ud <- newReadState dict
      bh <- return (setUserData bh ud)

      -- get the name cache from the ghc session
      ncRef <- withSession session (return . hsc_NC)
      nc <- readIORef ncRef

      -- get the symbol table
      symtab_p <- get bh
      data_p   <- tellBin bh
      seekBin bh symtab_p
      (nc', symtab) <- getSymbolTable bh nc
      seekBin bh data_p

      -- write back the new name cache
      writeIORef ncRef nc'

      -- set the symbol table
      let ud = getUserData bh
      bh <- return $! setUserData bh ud{ud_symtab = symtab}

      -- load the actual data
      iface <- get bh
      return (Right iface)


-------------------------------------------------------------------------------
-- Symbol table
-------------------------------------------------------------------------------

putSymbolTable :: BinHandle -> Int -> UniqFM (Int,Name) -> IO ()
putSymbolTable bh next_off symtab = do
  put_ bh next_off
  let names = elems (array (0,next_off-1) (eltsUFM symtab))
  mapM_ (\n -> serialiseName bh n symtab) names

getSymbolTable :: BinHandle -> NameCache -> IO (NameCache, Array Int Name)
getSymbolTable bh namecache = do
  sz <- get bh
  od_names <- sequence (replicate sz (get bh))
  let 
        arr = listArray (0,sz-1) names
        (namecache', names) =    
                mapAccumR (fromOnDiskName arr) namecache od_names
  --
  return (namecache', arr)

type OnDiskName = (PackageId, ModuleName, OccName)

fromOnDiskName
   :: Array Int Name
   -> NameCache
   -> OnDiskName
   -> (NameCache, Name)
fromOnDiskName arr nc (pid, mod_name, occ) =
  let 
        mod   = mkModule pid mod_name
        cache = nsNames nc
  in
  case lookupOrigNameCache cache  mod occ of
     Just name -> (nc, name)
     Nothing   -> 
        let 
                us        = nsUniqs nc
                uniq      = uniqFromSupply us
                name      = mkExternalName uniq mod occ noSrcSpan
                new_cache = extendNameCache cache mod occ name
        in        
        case splitUniqSupply us of { (us',_) -> 
        ( nc{ nsUniqs = us', nsNames = new_cache }, name )
        }

serialiseName :: BinHandle -> Name -> UniqFM (Int,Name) -> IO ()
serialiseName bh name symtab = do
  let mod = nameModule name
  put_ bh (modulePackageId mod, moduleName mod, nameOccName name)
