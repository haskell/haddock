{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.InterfaceFile
-- Copyright   :  (c) David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Reading and writing the .haddock interface file
-----------------------------------------------------------------------------
module Haddock.InterfaceFile (
  InterfaceFile(..), ifUnitId, ifModule,
  readInterfaceFile, nameCacheFromGhc, freshNameCache, NameCacheAccessor,
  writeInterfaceFile, binaryInterfaceVersion, binaryInterfaceVersionCompatibility
) where


import Haddock.Types
import Haddock.Utils hiding (out)

import Control.Monad
import Data.Array
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Word

import BinIface (getSymtabName, getDictFastString)
import Binary
import Binary.Unsafe (ioP, ioG)
import FastMutInt
import FastString
import GHC hiding (NoLink)
import GhcMonad (withSession)
import HscTypes
import NameCache
import IfaceEnv
import Name
import UniqFM
import UniqSupply
import Unique


data InterfaceFile = InterfaceFile {
  ifLinkEnv         :: LinkEnv,
  ifInstalledIfaces :: [InstalledInterface]
}


ifModule :: InterfaceFile -> Module
ifModule if_ =
  case ifInstalledIfaces if_ of
    [] -> error "empty InterfaceFile"
    iface:_ -> instMod iface

ifUnitId :: InterfaceFile -> UnitId
ifUnitId if_ =
  case ifInstalledIfaces if_ of
    [] -> error "empty InterfaceFile"
    iface:_ -> moduleUnitId $ instMod iface


binaryInterfaceMagic :: Word32
binaryInterfaceMagic = 0xD0Cface


-- IMPORTANT: Since datatypes in the GHC API might change between major
-- versions, and because we store GHC datatypes in our interface files, we need
-- to make sure we version our interface files accordingly.
--
-- If you change the interface file format or adapt Haddock to work with a new
-- major version of GHC (so that the format changes indirectly) *you* need to
-- follow these steps:
--
-- (1) increase `binaryInterfaceVersion`
--
-- (2) set `binaryInterfaceVersionCompatibility` to [binaryInterfaceVersion]
--
binaryInterfaceVersion :: Word16
#if (__GLASGOW_HASKELL__ >= 809) && (__GLASGOW_HASKELL__ < 811)
binaryInterfaceVersion = 34

binaryInterfaceVersionCompatibility :: [Word16]
binaryInterfaceVersionCompatibility = [binaryInterfaceVersion]
#else
#error Unsupported GHC version
#endif


writeInterfaceFile :: FilePath -> InterfaceFile -> IO ()
writeInterfaceFile filename iface = do
  bd <- runPutIO $ do

    put binaryInterfaceMagic
    put binaryInterfaceVersion

    -- remember where the dictionary pointer will go
    dict_p_p <- tellP
    put dict_p_p

    -- remember where the symbol table pointer will go
    symtab_p_p <- tellP
    put symtab_p_p

    -- Make some intial state
    symtab_next <- ioP $ newFastMutInt
    ioP $ writeFastMutInt symtab_next 0
    symtab_map <- ioP $ newIORef emptyUFM
    let bin_symtab = BinSymbolTable {
                        bin_symtab_next = symtab_next,
                        bin_symtab_map  = symtab_map }
    dict_next_ref <- ioP $ newFastMutInt
    ioP $ writeFastMutInt dict_next_ref 0
    dict_map_ref <- ioP $ newIORef emptyUFM
    let bin_dict = BinDictionary {
                        bin_dict_next = dict_next_ref,
                        bin_dict_map  = dict_map_ref }

    -- put the main thing
    writeState (putName bin_symtab)
               (putName bin_symtab)
               (putFastString bin_dict) $ do

      put iface

      -- write the symtab pointer at the front of the file
      symtab_p <- tellP
      putAt symtab_p_p symtab_p
      seekP symtab_p

      -- write the symbol table itself
      symtab_next' <- ioP $ readFastMutInt symtab_next
      symtab_map'  <- ioP $ readIORef symtab_map
      putSymbolTable symtab_next' symtab_map'

      -- write the dictionary pointer at the fornt of the file
      dict_p <- tellP
      putAt dict_p_p dict_p
      seekP dict_p

      -- write the dictionary itself
      dict_next <- ioP $ readFastMutInt dict_next_ref
      dict_map  <- ioP $ readIORef dict_map_ref
      putDictionary dict_next dict_map

  -- and send the result to the file
  writeBinData bd filename


type NameCacheAccessor m = (m NameCache, NameCache -> m ())


nameCacheFromGhc :: forall m. (GhcMonad m, MonadIO m) => NameCacheAccessor m
nameCacheFromGhc = ( read_from_session , write_to_session )
  where
    read_from_session = do
       ref <- withSession (return . hsc_NC)
       liftIO $ readIORef ref
    write_to_session nc' = do
       ref <- withSession (return . hsc_NC)
       liftIO $ writeIORef ref nc'


freshNameCache :: NameCacheAccessor IO
freshNameCache = ( create_fresh_nc , \_ -> return () )
  where
    create_fresh_nc = do
       u  <- mkSplitUniqSupply 'a' -- ??
       return (initNameCache u [])


-- | Read a Haddock (@.haddock@) interface file. Return either an
-- 'InterfaceFile' or an error message.
--
-- This function can be called in two ways.  Within a GHC session it will
-- update the use and update the session's name cache.  Outside a GHC session
-- a new empty name cache is used.  The function is therefore generic in the
-- monad being used.  The exact monad is whichever monad the first
-- argument, the getter and setter of the name cache, requires.
--
readInterfaceFile :: forall m.
                     MonadIO m
                  => NameCacheAccessor m
                  -> FilePath
                  -> Bool  -- ^ Disable version check. Can cause runtime crash.
                  -> m (Either String InterfaceFile)
readInterfaceFile (get_name_cache, set_name_cache) filename bypass_checks = do
  nc_var <- get_name_cache >>= (liftIO . newIORef)

  bd <- liftIO $ readBinData filename

  x <- liftIO . runGetIO bd $ do
    magic   <- get
    version <- get

    case () of
      _ | magic /= binaryInterfaceMagic -> return . Left $
        "Magic number mismatch: couldn't load interface file: " ++ filename
        | not bypass_checks
        , (version `notElem` binaryInterfaceVersionCompatibility) -> return . Left $
        "Interface file is of wrong version: " ++ filename
        | otherwise -> do

          dict <- get_dictionary

          -- read the symbol table so we are capable of reading the actual data
          readState (error "getSymtabName") (getDictFastString dict) $ do
            symtab <- update_nc nc_var ioG get_symbol_table
            readState (getSymtabName (NCU (\f -> update_nc nc_var id (return . f))) dict symtab)
                      (getDictFastString dict) $

              -- load the actual data
              Right <$> get

  liftIO (readIORef nc_var) >>= set_name_cache

  return x

 where
   update_nc :: forall n b. Monad n
             => IORef NameCache
             -> (forall a. IO a -> n a)
             -> (NameCache -> n (NameCache, b))
             -> n b
   update_nc nc_var lift f = do
     nc <- lift $ readIORef nc_var
     (nc', x) <- f nc
     lift $ writeIORef nc_var nc'
     return x

   get_dictionary = do
      dict_p <- get
      data_p <- tellG
      seekG dict_p
      dict <- getDictionary
      seekG data_p
      return dict

   get_symbol_table theNC = do
      symtab_p <- get
      data_p'  <- tellG
      seekG symtab_p
      (nc', symtab) <- getSymbolTable theNC
      seekG data_p'
      return (nc', symtab)


-------------------------------------------------------------------------------
-- * Symbol table
-------------------------------------------------------------------------------


putName :: BinSymbolTable -> Name -> Put ()
putName BinSymbolTable{
            bin_symtab_map = symtab_map_ref,
            bin_symtab_next = symtab_next }    name
  = do
    symtab_map <- ioP $ readIORef symtab_map_ref
    case lookupUFM symtab_map name of
      Just (off,_) -> put (fromIntegral off :: Word32)
      Nothing -> do
         off <- ioP $ readFastMutInt symtab_next
         ioP $ writeFastMutInt symtab_next (off+1)
         ioP $ writeIORef symtab_map_ref
             $! addToUFM symtab_map name (off,name)
         put (fromIntegral off :: Word32)


data BinSymbolTable = BinSymbolTable {
        bin_symtab_next :: !FastMutInt, -- The next index to use
        bin_symtab_map  :: !(IORef (UniqFM (Int,Name)))
                                -- indexed by Name
  }


putFastString :: BinDictionary -> FastString -> Put ()
putFastString BinDictionary { bin_dict_next = j_r,
                              bin_dict_map  = out_r}  f
  = do
    out <- ioP $ readIORef out_r
    let unique = getUnique f
    case lookupUFM out unique of
        Just (j, _)  -> put (fromIntegral j :: Word32)
        Nothing -> do
           j <- ioP $ readFastMutInt j_r
           put (fromIntegral j :: Word32)
           ioP $ writeFastMutInt j_r (j + 1)
           ioP $ writeIORef out_r $! addToUFM out unique (j, f)


data BinDictionary = BinDictionary {
        bin_dict_next :: !FastMutInt, -- The next index to use
        bin_dict_map  :: !(IORef (UniqFM (Int,FastString)))
                                -- indexed by FastString
  }


putSymbolTable :: Int -> UniqFM (Int,Name) -> Put ()
putSymbolTable next_off symtab = do
  put next_off
  let names = elems (array (0,next_off-1) (eltsUFM symtab))
  mapM_ (\n -> serialiseName n symtab) names


getSymbolTable :: NameCache -> Get (NameCache, Array Int Name)
getSymbolTable namecache = do
  sz <- get
  od_names <- replicateM sz get
  let arr = listArray (0,sz-1) names
      (namecache', names) = mapAccumR (fromOnDiskName arr) namecache od_names
  return (namecache', arr)


type OnDiskName = (UnitId, ModuleName, OccName)


fromOnDiskName
   :: Array Int Name
   -> NameCache
   -> OnDiskName
   -> (NameCache, Name)
fromOnDiskName _ nc (pid, mod_name, occ) =
  let
        modu  = mkModule pid mod_name
        cache = nsNames nc
  in
  case lookupOrigNameCache cache modu occ of
     Just name -> (nc, name)
     Nothing   ->
        let
                us        = nsUniqs nc
                u         = uniqFromSupply us
                name      = mkExternalName u modu occ noSrcSpan
                new_cache = extendNameCache cache modu occ name
        in
        case splitUniqSupply us of { (us',_) ->
        ( nc{ nsUniqs = us', nsNames = new_cache }, name )
        }


serialiseName :: Name -> UniqFM (Int,Name) -> Put ()
serialiseName name _ = do
  let modu = nameModule name
  put (moduleUnitId modu, moduleName modu, nameOccName name)


-------------------------------------------------------------------------------
-- * GhcBinary instances
-------------------------------------------------------------------------------


instance (Ord k, Binary k, Binary v) => Binary (Map k v) where
  put = put . Map.toList
  get = Map.fromList <$> get


instance Binary InterfaceFile where
  put (InterfaceFile env ifaces) = do
    put env
    put ifaces

  get = InterfaceFile <$> get <*> get


instance Binary InstalledInterface where
  put (InstalledInterface modu is_sig info docMap argMap
           exps visExps opts fixMap) = do
    put modu
    put is_sig
    put info
    lazyPut (docMap, argMap)
    put exps
    put visExps
    put opts
    put fixMap

  get = do
    modu    <- get
    is_sig  <- get
    info    <- get
    ~(docMap, argMap) <- lazyGet
    exps    <- get
    visExps <- get
    opts    <- get
    fixMap  <- get
    return (InstalledInterface modu is_sig info docMap argMap
            exps visExps opts fixMap)


instance Binary DocOption where
    put OptHide           = putByte 0
    put OptPrune          = putByte 1
    put OptIgnoreExports  = putByte 2
    put OptNotHome        = putByte 3
    put OptShowExtensions = putByte 4
    get = do
            h <- getByte
            case h of
              0 -> do
                    return OptHide
              1 -> do
                    return OptPrune
              2 -> do
                    return OptIgnoreExports
              3 -> do
                    return OptNotHome
              4 -> do
                    return OptShowExtensions
              _ -> fail "invalid binary data found"


instance Binary Example where
    put (Example expression result) = do
        put expression
        put result
    get = Example <$> get <*> get

instance Binary a => Binary (Hyperlink a) where
    put (Hyperlink url label) = do
        put url
        put label
    get = Hyperlink <$> get <*> get

instance Binary Picture where
    put (Picture uri title) = do
        put uri
        put title
    get = Picture <$> get <*> get

instance Binary a => Binary (Header a) where
    put (Header l t) = do
        put l
        put t
    get = Header <$> get <*> get

instance Binary a => Binary (Table a) where
    put (Table h b) = do
        put h
        put b
    get = Table <$> get <*> get

instance Binary a => Binary (TableRow a) where
    put (TableRow cs) = put cs
    get = TableRow <$> get

instance Binary a => Binary (TableCell a) where
    put (TableCell i j c) = do
        put i
        put j
        put c
    get = TableCell <$> get <*> get <*> get

instance Binary Meta where
    put (Meta v p) = do
        put v
        put p
    get = Meta <$> get <*> get

instance (Binary mod, Binary id) => Binary (MetaDoc mod id) where
  put MetaDoc { _meta = m, _doc = d } = do
    put m
    put d
  get = do
    m <- get
    d <- get
    return $ MetaDoc { _meta = m, _doc = d }

instance (Binary mod, Binary id) => Binary (DocH mod id) where
    put DocEmpty = do
            putByte 0
    put (DocAppend aa ab) = do
            putByte 1
            put aa
            put ab
    put (DocString ac) = do
            putByte 2
            put ac
    put (DocParagraph ad) = do
            putByte 3
            put ad
    put (DocIdentifier ae) = do
            putByte 4
            put ae
    put (DocModule af) = do
            putByte 5
            put af
    put (DocEmphasis ag) = do
            putByte 6
            put ag
    put (DocMonospaced ah) = do
            putByte 7
            put ah
    put (DocUnorderedList ai) = do
            putByte 8
            put ai
    put (DocOrderedList aj) = do
            putByte 9
            put aj
    put (DocDefList ak) = do
            putByte 10
            put ak
    put (DocCodeBlock al) = do
            putByte 11
            put al
    put (DocHyperlink am) = do
            putByte 12
            put am
    put (DocPic x) = do
            putByte 13
            put x
    put (DocAName an) = do
            putByte 14
            put an
    put (DocExamples ao) = do
            putByte 15
            put ao
    put (DocIdentifierUnchecked x) = do
            putByte 16
            put x
    put (DocWarning ag) = do
            putByte 17
            put ag
    put (DocProperty x) = do
            putByte 18
            put x
    put (DocBold x) = do
            putByte 19
            put x
    put (DocHeader aa) = do
            putByte 20
            put aa
    put (DocMathInline x) = do
            putByte 21
            put x
    put (DocMathDisplay x) = do
            putByte 22
            put x
    put (DocTable x) = do
            putByte 23
            put x

    get = do
            h <- getByte
            case h of
              0 -> do
                    return DocEmpty
              1 -> do
                    aa <- get
                    ab <- get
                    return (DocAppend aa ab)
              2 -> do
                    ac <- get
                    return (DocString ac)
              3 -> do
                    ad <- get
                    return (DocParagraph ad)
              4 -> do
                    ae <- get
                    return (DocIdentifier ae)
              5 -> do
                    af <- get
                    return (DocModule af)
              6 -> do
                    ag <- get
                    return (DocEmphasis ag)
              7 -> do
                    ah <- get
                    return (DocMonospaced ah)
              8 -> do
                    ai <- get
                    return (DocUnorderedList ai)
              9 -> do
                    aj <- get
                    return (DocOrderedList aj)
              10 -> do
                    ak <- get
                    return (DocDefList ak)
              11 -> do
                    al <- get
                    return (DocCodeBlock al)
              12 -> do
                    am <- get
                    return (DocHyperlink am)
              13 -> do
                    x <- get
                    return (DocPic x)
              14 -> do
                    an <- get
                    return (DocAName an)
              15 -> do
                    ao <- get
                    return (DocExamples ao)
              16 -> do
                    x <- get
                    return (DocIdentifierUnchecked x)
              17 -> do
                    ag <- get
                    return (DocWarning ag)
              18 -> do
                    x <- get
                    return (DocProperty x)
              19 -> do
                    x <- get
                    return (DocBold x)
              20 -> do
                    aa <- get
                    return (DocHeader aa)
              21 -> do
                    x <- get
                    return (DocMathInline x)
              22 -> do
                    x <- get
                    return (DocMathDisplay x)
              23 -> do
                    x <- get
                    return (DocTable x)
              _ -> error "invalid binary data found in the interface file"


instance Binary name => Binary (HaddockModInfo name) where
  put hmi = do
    put (hmi_description hmi)
    put (hmi_copyright   hmi)
    put (hmi_license     hmi)
    put (hmi_maintainer  hmi)
    put (hmi_stability   hmi)
    put (hmi_portability hmi)
    put (hmi_safety      hmi)
    put (fromEnum <$> hmi_language hmi)
    put (map fromEnum $ hmi_extensions hmi)

  get = do
    descr <- get
    copyr <- get
    licen <- get
    maint <- get
    stabi <- get
    porta <- get
    safet <- get
    langu <- fmap toEnum <$> get
    exten <- map toEnum <$> get
    return (HaddockModInfo descr copyr licen maint stabi porta safet langu exten)

instance Binary DocName where
  put (Documented name modu) = do
    putByte 0
    put name
    put modu
  put (Undocumented name) = do
    putByte 1
    put name

  get = do
    h <- getByte
    case h of
      0 -> do
        name <- get
        modu <- get
        return (Documented name modu)
      1 -> do
        name <- get
        return (Undocumented name)
      _ -> error "get DocName: Bad h"
