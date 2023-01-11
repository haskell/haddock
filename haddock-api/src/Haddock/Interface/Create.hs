{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wwarn #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.Create
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a single function 'createInterface',
-- which creates a Haddock 'Interface' from the typechecking
-- results 'TypecheckedModule' from GHC.
-----------------------------------------------------------------------------
module Haddock.Interface.Create (IfM, runIfM, createInterface1) where

import Haddock.Convert (PrintRuntimeReps (..), tyThingToLHsDecl)
import Haddock.GhcUtils (addClassContext, lHsQTyVarsToTypes,
                         mkEmptySigType, moduleString,
                         pretty, restrictTo, sigName, dataTupleModule, dataListModule)
import Haddock.Interface.LexParseRn
import Haddock.Options (Flag (..), modulePackageInfo)
import Haddock.Types hiding (liftErrMsg)
import Haddock.Utils (replace)
import Documentation.Haddock.Doc

import Control.Monad.Reader (MonadReader (..), ReaderT, asks, runReaderT)
import Control.Monad.Writer.Strict hiding (tell)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Traversable (for)
import qualified Data.List.NonEmpty as NE
import Data.Foldable
import qualified Data.Map as Map

import GHC hiding (lookupName)
import qualified GHC.Types.Unique.Map as UniqMap
import GHC.Core.ConLike (ConLike (..))
import GHC.Data.FastString (unpackFS, bytesFS)
import GHC.HsToCore.Docs hiding (mkMaps)
import GHC.Types.Avail hiding (avail)
import qualified GHC.Types.Avail as Avail
import GHC.Types.Basic
import GHC.Types.Name (getOccString, getSrcSpan, isDataConName, isValName, nameIsLocalOrFrom, nameOccName)
import GHC.Types.Name.Set
import qualified GHC.Types.SrcLoc as SrcLoc
import GHC.Unit.State (PackageName (..), UnitState)
import qualified GHC.Utils.Outputable as O
import GHC.Utils.Panic (pprPanic)
import GHC.Unit.Module.Warnings
import GHC.Driver.Ppr
import GHC.Unit.Module.ModIface
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Types.SafeHaskell
import Control.Arrow ((&&&))
import GHC.Types.Name.Occurrence
import GHC.Iface.Syntax
import GHC.Types.SourceText

newtype IfEnv m = IfEnv
  {
    -- | Lookup names in the enviroment.
    ife_lookup_name :: Name -> m (Maybe TyThing)
  }


-- | A monad in which we create Haddock interfaces. Not to be confused with
-- `GHC.Tc.Types.IfM` which is used to write GHC interfaces.
--
-- In the past `createInterface` was running in the `Ghc` monad but proved hard
-- to sustain as soon as we moved over for Haddock to be a plugin. Also abstracting
-- over the Ghc specific clarifies where side effects happen.
newtype IfM m a = IfM { unIfM :: ReaderT (IfEnv m) (WriterT [ErrMsg] m) a }


deriving newtype instance Functor m => Functor (IfM m)
deriving newtype instance Applicative m => Applicative (IfM m)
deriving newtype instance Monad m => Monad (IfM m)
deriving newtype instance MonadIO m => MonadIO (IfM m)
deriving newtype instance Monad m => MonadReader (IfEnv m) (IfM m)
deriving newtype instance Monad m => MonadWriter [ErrMsg] (IfM m)


-- | Run an `IfM` action.
runIfM
  -- | Lookup a global name in the current session. Used in cases
  -- where declarations don't
  :: (Name -> m (Maybe TyThing))
  -- | The action to run.
  -> IfM m a
  -- | Result and accumulated error/warning messages.
  -> m (a, [ErrMsg])
runIfM lookup_name action = do
  let
    if_env = IfEnv
      {
        ife_lookup_name = lookup_name
      }
  runWriterT (runReaderT (unIfM action) if_env)


liftErrMsg :: Monad m => ErrMsgM a -> IfM m a
liftErrMsg action = do
  writer (runWriter action)


lookupName :: Monad m => Name -> IfM m (Maybe TyThing)
lookupName name = IfM $ do
  lookup_name <- asks ife_lookup_name
  lift $ lift (lookup_name name)


createInterface1
  :: MonadIO m
  => [Flag]
  -> UnitState
  -> ModSummary
  -> ModIface
  -> IfaceMap
  -> InstIfaceMap
  -> ([ClsInst],[FamInst])
  -> IfM m Interface
createInterface1 flags unit_state mod_sum mod_iface ifaces inst_ifaces (instances, fam_instances) = do

  let
    ModSummary
      {
        -- Cached flags from OPTIONS, INCLUDE and LANGUAGE
        -- pragmas in the modules source code. Used to infer
        -- safety of module.
        ms_hspp_opts
      , ms_location = ModLocation
        {
          ml_hie_file
        }
      } = mod_sum

    dflags  = ms_hspp_opts
    mdl     = mi_module mod_iface
    sem_mdl = mi_semantic_module mod_iface
    is_sig  = isJust (mi_sig_of mod_iface)
    safety  = getSafeMode (mi_trust mod_iface)

    (pkg_name_fs, _) =
      modulePackageInfo unit_state flags (Just mdl)

    pkg_name :: Maybe Package
    pkg_name =
      let
        unpack (PackageName name) = unpackFS name
      in
        fmap unpack pkg_name_fs

    warnings = mi_warns mod_iface

    -- See Note [Exporting built-in items]
    special_exports
      | mdl == gHC_TYPES  = listAvail <> eqAvail
      | mdl == gHC_PRIM   = funAvail
      | mdl == pRELUDE    = listAvail <> funAvail
      | mdl == dataTupleModule = tupsAvail
      | mdl == dataListModule  = listAvail
      | otherwise         = []
    !exportedNames = concatMap availNamesWithSelectors
                               (special_exports <> mi_exports mod_iface)

    fixities :: FixMap
    fixities = mkFixMap exportedNames (mi_fixities mod_iface)

    -- This is used for looking up the Name of a default method
    -- from its OccName. See Note [default method Name] in GHC.Iface.Recomp
    def_meths_env = mkOccEnv def_meths
    def_meths = [ (nameOccName nm, nm)
                | (_, IfaceId { ifName = nm }) <- mi_decls mod_iface
                , let occ = nameOccName nm
                , isDefaultMethodOcc occ
                ]

  mod_iface_docs <- case mi_docs mod_iface of
    Just docs -> pure docs
    Nothing -> do
      liftErrMsg $ tell [showPpr dflags mdl ++ " has no docs in its .hi-file"]
      pure emptyDocs
  -- Derive final options to use for haddocking this module
  doc_opts <- liftErrMsg $ mkDocOpts (docs_haddock_opts mod_iface_docs) flags mdl

  let prr | OptPrintRuntimeRep `elem` doc_opts = ShowRuntimeRep
          | otherwise = HideRuntimeRep

  (!info, header_doc) <- liftErrMsg $ processModuleHeader dflags pkg_name safety
                                        (docs_language mod_iface_docs)
                                        (docs_extensions mod_iface_docs)
                                        (docs_mod_hdr mod_iface_docs)
  mod_warning <- liftErrMsg $ moduleWarning dflags warnings

  (docMap :: DocMap Name) <- do 
    let docsDecls = Map.fromList $ UniqMap.nonDetEltsUniqMap mod_iface_docs.docs_decls
    traverse (liftErrMsg <$> processDocStringsParas dflags pkg_name) docsDecls

  (argMap :: Map Name (Map Int (MDoc Name))) <- do
      let docsArgs = Map.fromList $ UniqMap.nonDetEltsUniqMap mod_iface_docs.docs_args
      (result :: Map Name (IntMap (MDoc Name))) <- 
          traverse (traverse (liftErrMsg . processDocStringParas dflags pkg_name)) docsArgs
      let result2 = Map.map (\intMap -> Map.fromList $ IM.assocs intMap) result
      pure $ result2

  warningMap <- liftErrMsg $ mkWarningMap dflags warnings exportedNames

  let local_instances = filter (nameIsLocalOrFrom sem_mdl)
                        $  map getName instances
                        ++ map getName fam_instances
      instanceMap = M.fromList [(l, n) | n <- local_instances, RealSrcSpan l _ <- [getSrcSpan n] ]

  -- See Note [Exporting built-in items]
  let builtinTys = DsiSectionHeading 1 (WithHsDocIdentifiers (mkGeneratedHsDocString "Builtin syntax") [])
      bonus_ds mods
        | mdl == gHC_TYPES  = [ DsiExports (listAvail <> eqAvail) ] <> mods
        | mdl == gHC_PRIM   = [ builtinTys, DsiExports funAvail ] <> mods
        | mdl == pRELUDE    = let (hs, rest) = splitAt 2 mods
                              in hs <> [ DsiExports (listAvail <> funAvail) ] <> rest
        | mdl == dataTupleModule = mods <> [ DsiExports tupsAvail ]
        | mdl == dataListModule  = [ DsiExports listAvail ] <> mods
        | otherwise         = mods

  let
    -- Warnings in this module and transitive warnings from dependend modules
    transitiveWarnings :: Map Name (Doc Name)
    transitiveWarnings = M.unions (warningMap : map ifaceWarningMap (M.elems ifaces))

  export_items <- mkExportItems
    prr
    ifaces
    pkg_name
    mdl
    transitiveWarnings
    docMap
    argMap
    fixities
    (docs_named_chunks mod_iface_docs)
    (bonus_ds $ docs_structure mod_iface_docs)
    inst_ifaces
    dflags
    def_meths_env

  let
    visible_names :: [Name]
    visible_names = mkVisibleNames instanceMap export_items doc_opts

    -- Measure haddock documentation coverage.
    pruned_export_items :: [ExportItem GhcRn]
    pruned_export_items = pruneExportItems export_items

    !haddockable = 1 + length export_items -- module + exports
    !haddocked = (if isJust header_doc then 1 else 0) + length pruned_export_items

    coverage :: (Int, Int)
    !coverage = (haddockable, haddocked)

  return $! Interface
    {
      ifaceMod               = mdl
    , ifaceIsSig             = is_sig
    , ifaceHieFile           = ml_hie_file
    , ifaceInfo              = info
    , ifaceDoc               = Documentation header_doc mod_warning
    , ifaceRnDoc             = Documentation Nothing Nothing
    , ifaceOptions           = doc_opts
    , ifaceDocMap            = docMap
    , ifaceArgMap            = argMap
    , ifaceRnDocMap          = M.empty
    , ifaceRnArgMap          = M.empty
    , ifaceExportItems       = if OptPrune `elem` doc_opts then
                                 pruned_export_items else export_items
    , ifaceRnExportItems     = []
    , ifaceExports           = exportedNames
    , ifaceVisibleExports    = visible_names
    , ifaceFixMap            = fixities
    , ifaceInstances         = instances
    , ifaceFamInstances      = fam_instances
    , ifaceOrphanInstances   = [] -- Filled in attachInstances
    , ifaceRnOrphanInstances = [] -- Filled in attachInstances
    , ifaceHaddockCoverage   = coverage
    , ifaceWarningMap        = warningMap
    , ifaceDynFlags          = dflags
    , ifaceDefMeths          = def_meths
    }
  where
    -- Note [Exporting built-in items]
    --
    -- Some items do not show up in their modules exports simply because Haskell
    -- lacks the concrete syntax to represent such an export. We'd still like
    -- these to show up in docs, so we manually patch on some extra exports for a
    -- small number of modules:
    --
    --   * "GHC.Prim" should export @(->)@
    --   * "GHC.Types" should export @[]([], (:))@ and @(~)@
    --   * "Prelude" should export @(->)@ and @[]([], (:))@
    --   * "Data.Tuple" should export tuples up to arity 15 (that is the number
    --     that Haskell98 guarantees exist and that is also the point at which
    --     GHC stops providing instances)
    --
    listAvail = [ availTC listTyConName
                          [listTyConName, nilDataConName, consDataConName]
                          [] ]
    funAvail  = [ availTC funTyConName [funTyConName] [] ]
    eqAvail   = [ availTC eqTyConName [eqTyConName] [] ]
    tupsAvail = [ availTC tyName [tyName, datName] []
                | i<-[0..15]
                , let tyName = tupleTyConName BoxedTuple i
                , let datName = getName $ tupleDataCon Boxed i
                ]


-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

mkWarningMap :: DynFlags -> Warnings GhcRn -> [Name] -> ErrMsgM WarningMap
mkWarningMap dflags warnings exps = case warnings of
  NoWarnings  -> pure M.empty
  WarnAll _   -> pure M.empty
  WarnSome ws ->
    let expsOccEnv = mkOccEnv [(nameOccName n, n) | n <- exps]
        ws' = flip mapMaybe ws $ \(occ, w) ->
                (,w) <$> lookupOccEnv expsOccEnv occ
    in M.fromList <$> traverse (traverse (parseWarning dflags)) ws'

moduleWarning :: DynFlags -> Warnings GhcRn -> ErrMsgM (Maybe (Doc Name))
moduleWarning dflags = \case
  NoWarnings -> pure Nothing
  WarnSome _ -> pure Nothing
  WarnAll w  -> Just <$> parseWarning dflags w

parseWarning :: DynFlags -> WarningTxt GhcRn -> ErrMsgM (Doc Name)
parseWarning dflags w = case w of
  DeprecatedTxt _ msg -> format "Deprecated: " (map (dstToDoc . unLoc) msg)
  WarningTxt    _ msg -> format "Warning: "    (map (dstToDoc . unLoc) msg)
  where
    dstToDoc (WithHsDocIdentifiers st ids) = WithHsDocIdentifiers (stToDoc st) ids
    stToDoc (StringLiteral _ fs _) = GeneratedDocString $ HsDocStringChunk (bytesFS fs)
    format x bs = DocWarning . DocParagraph . DocAppend (DocString x)
                  <$> foldrM (\doc rest -> docAppend <$> processDocString dflags doc <*> pure rest) DocEmpty bs
-------------------------------------------------------------------------------
-- Doc options
--
-- Haddock options that are embedded in the source file
-------------------------------------------------------------------------------


mkDocOpts :: Maybe String -> [Flag] -> Module -> ErrMsgM [DocOption]
mkDocOpts mbOpts flags mdl = do
  opts <- case mbOpts of
    Just opts -> case words $ replace ',' ' ' opts of
      [] -> tell ["No option supplied to DOC_OPTION/doc_option"] >> return []
      xs -> liftM catMaybes (mapM parseOption xs)
    Nothing -> return []
  pure (foldl go opts flags)
  where
    mdlStr = moduleString mdl

    -- Later flags override earlier ones
    go os m | m == Flag_HideModule mdlStr     = OptHide : os
            | m == Flag_ShowModule mdlStr     = filter (/= OptHide) os
            | m == Flag_ShowAllModules        = filter (/= OptHide) os
            | m == Flag_ShowExtensions mdlStr = OptShowExtensions : os
            | otherwise                       = os

parseOption :: String -> ErrMsgM (Maybe DocOption)
parseOption "hide"            = return (Just OptHide)
parseOption "prune"           = return (Just OptPrune)
parseOption "not-home"        = return (Just OptNotHome)
parseOption "show-extensions" = return (Just OptShowExtensions)
parseOption "print-explicit-runtime-reps" = return (Just OptPrintRuntimeRep)
parseOption other = tell ["Unrecognised option: " ++ other] >> return Nothing


--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------



-- | Extract a map of fixity declarations only
mkFixMap :: [Name] -> [(OccName, Fixity)] -> FixMap
mkFixMap exps occFixs =
    M.fromList $ flip mapMaybe occFixs $ \(occ, fix_) ->
      (,fix_) <$> lookupOccEnv expsOccEnv occ
    where
      expsOccEnv = mkOccEnv (map (nameOccName &&& id) exps)


-- | Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.
--
-- We create the export items even if the module is hidden, since they
-- might be useful when creating the export items for other modules.
mkExportItems
  :: forall m. Monad m
  => PrintRuntimeReps
  -> IfaceMap
  -> Maybe Package      -- this package
  -> Module             -- this module
  -> WarningMap
  -> DocMap Name
  -> ArgMap Name
  -> FixMap
  -> Map String (HsDoc GhcRn) -- named chunks
  -> DocStructure
  -> InstIfaceMap
  -> DynFlags
  -> OccEnv Name
  -> IfM m [ExportItem GhcRn]
mkExportItems
  prr modMap pkgName thisMod warnings docMap argMap fixMap namedChunks dsItems
  instIfaceMap dflags defMeths =
    concat <$> traverse lookupExport dsItems
  where
    lookupExport :: DocStructureItem -> IfM m [ExportItem GhcRn]
    lookupExport = \case
      DsiSectionHeading lev hsDoc' -> do
        doc <- liftErrMsg $ processDocString dflags hsDoc'
        pure [ExportGroup lev "" doc]
      DsiDocChunk hsDoc' -> do
        doc <- liftErrMsg $ processDocStringParas dflags pkgName hsDoc'
        pure [ExportDoc doc]
      DsiNamedChunkRef ref -> do
        case M.lookup ref namedChunks of
          Nothing -> do
            liftErrMsg $ tell ["Cannot find documentation for: $" ++ ref]
            pure []
          Just hsDoc' -> do
            doc <- liftErrMsg $ processDocStringParas dflags pkgName hsDoc'
            pure [ExportDoc doc]
      DsiExports avails ->
        -- TODO: We probably don't need nubAvails here.
        -- mkDocStructureFromExportList already uses it.
        concat <$> traverse availExport (nubAvails avails)
      DsiModExport mod_names avails -> do
        -- only consider exporting a module if we are sure we are really
        -- exporting the whole module and not some subset.
        (unrestricted_mods, remaining_avails) <- unrestrictedModExports dflags thisMod modMap instIfaceMap avails (NE.toList mod_names)
        avail_exps <- concat <$> traverse availExport remaining_avails
        pure (map ExportModule unrestricted_mods ++ avail_exps)

    availExport avail =
      availExportItem prr modMap thisMod warnings
        docMap argMap fixMap instIfaceMap dflags avail defMeths


unrestrictedModExports
  :: Monad m
  => DynFlags
  -> Module           -- ^ Current Module
  -> IfaceMap         -- ^ Already created interfaces
  -> InstIfaceMap     -- ^ Interfaces in other packages
  -> Avails           -- ^ Modules to be exporte
  -> [ModuleName]
  -> IfM m ([Module], Avails)
     -- ^ ( modules exported without restriction
     --   , remaining exports not included in any
     --     of these modules
     --   )
unrestrictedModExports dflags thisMod ifaceMap instIfaceMap avails mod_names = do
    mods_and_exports <- fmap catMaybes $ for mod_names $ \mod_name -> do
      let m_local = mkModule (moduleUnit thisMod) mod_name
      case M.lookup m_local ifaceMap of
        -- First lookup locally
        Just iface -> pure $ Just (ifaceMod iface, mkNameSet (ifaceExports iface))
        Nothing ->
          case M.lookup mod_name instIfaceMap' of
            Just iface -> pure $ Just (instMod iface, mkNameSet (instExports iface))
            Nothing -> do
              liftErrMsg $ tell ["Warning: " ++ pretty dflags thisMod ++ ": Could not find " ++
                                 "documentation for exported module: " ++ pretty dflags mod_name]
              pure Nothing
    let unrestricted = filter everythingVisible mods_and_exports
        mod_exps = unionNameSets (map snd unrestricted)
        remaining = nubAvails (filterAvails (\n -> not (n `elemNameSet` mod_exps)) avails)
    pure (map fst unrestricted, remaining)
  where
    instIfaceMap' = (M.mapKeys moduleName instIfaceMap)
    all_names = availsToNameSetWithSelectors avails

    -- Is everything in this (supposedly re-exported) module visible?
    everythingVisible :: (Module, NameSet) -> Bool
    everythingVisible (mdl, exps)
      | not (exps `isSubsetOf` all_names) = False
      | Just iface <- M.lookup mdl ifaceMap = OptHide `notElem` ifaceOptions iface
      | Just iface <- M.lookup (moduleName mdl) instIfaceMap' = OptHide `notElem` instOptions iface
      | otherwise = True

    -- TODO: Add a utility based on IntMap.isSubmapOfBy
    isSubsetOf :: NameSet -> NameSet -> Bool
    isSubsetOf a b = nameSetAll (`elemNameSet` b) a

availExportItem
  :: forall m
  .  Monad m
  => PrintRuntimeReps
  -> IfaceMap
  -> Module             -- this module
  -> WarningMap
  -> Map Name (MDoc Name)        -- docs (keyed by 'Name's)
  -> ArgMap Name        -- docs for arguments (keyed by 'Name's)
  -> FixMap
  -> InstIfaceMap
  -> DynFlags
  -> AvailInfo
  -> OccEnv Name       -- Default methods
  -> IfM m [ExportItem GhcRn]
availExportItem prr modMap thisMod warnings docMap argMap fixMap instIfaceMap
  dflags availInfo defMeths = declWith availInfo
  where
    declWith :: AvailInfo -> IfM m [ ExportItem GhcRn ]
    declWith avail = do
      let t = availName avail
      mayDecl <- hiDecl dflags prr t
      case mayDecl of
        Nothing -> return [ ExportNoDecl t [] ]
        Just decl -> do
          availExportDecl avail decl =<< do
            -- Find docs for decl
            let tmod = nameModule t
            if tmod == thisMod
            then pure (lookupDocs avail warnings docMap argMap defMeths)
            else case M.lookup tmod modMap of
              Just iface ->
                pure (lookupDocs avail warnings (ifaceDocMap iface) (ifaceArgMap iface) (mkOccEnv (ifaceDefMeths iface)))
              Nothing ->
                -- We try to get the subs and docs
                -- from the installed .haddock file for that package.
                -- TODO: This needs to be more sophisticated to deal
                -- with signature inheritance
                case M.lookup (nameModule t) instIfaceMap of
                  Nothing -> do
                     liftErrMsg $ tell
                        ["Warning: " ++ pretty dflags thisMod ++
                         ": Couldn't find .haddock for export " ++ pretty dflags t]
                     let subs_ = availNoDocs avail
                     pure (noDocForDecl, subs_)
                  Just instIface ->
                    pure (lookupDocs avail warnings (instDocMap instIface) (instArgMap instIface) (mkOccEnv (instDefMeths instIface)))


    -- Tries 'extractDecl' first then falls back to 'hiDecl' if that fails
    availDecl :: Name -> LHsDecl GhcRn -> IfM m (LHsDecl GhcRn)
    availDecl declName parentDecl = extractDecl prr dflags declName parentDecl >>= \case
        Right d -> pure d
        Left err -> do
          synifiedDeclOpt <- hiDecl dflags prr declName
          case synifiedDeclOpt of
            Just synifiedDecl -> pure synifiedDecl
            Nothing -> pprPanic "availExportItem" (O.text err)

    availExportDecl :: AvailInfo -> LHsDecl GhcRn
                    -> (DocForDecl Name, [(Name, DocForDecl Name)])
                    -> IfM m [ ExportItem GhcRn ]
    availExportDecl avail decl (doc, subs)
      | availExportsDecl avail = do
          extractedDecl <- availDecl (availName avail) decl

          -- bundled pattern synonyms only make sense if the declaration is
          -- exported (otherwise there would be nothing to bundle to)
          bundledPatSyns <- findBundledPatterns avail

          let
            patSynNames =
              concatMap (getMainDeclBinder emptyOccEnv . fst) bundledPatSyns

            fixities =
                [ (n, f)
                | n <- availName avail : fmap fst subs ++ patSynNames
                , Just f <- [M.lookup n fixMap]
                ]

          return [ ExportDecl {
                       expItemDecl      = restrictTo (fmap fst subs) extractedDecl
                     , expItemPats      = bundledPatSyns
                     , expItemMbDoc     = doc
                     , expItemSubDocs   = subs
                     , expItemInstances = []
                     , expItemFixities  = fixities
                     , expItemSpliced   = False
                     }
                 ]

      | otherwise = for subs $ \(sub, sub_doc) -> do
          extractedDecl <- availDecl sub decl

          return ( ExportDecl {
                       expItemDecl      = extractedDecl
                     , expItemPats      = []
                     , expItemMbDoc     = sub_doc
                     , expItemSubDocs   = []
                     , expItemInstances = []
                     , expItemFixities  = [ (sub, f) | Just f <- [M.lookup sub fixMap] ]
                     , expItemSpliced   = False
                     } )

    findBundledPatterns :: AvailInfo -> IfM m [(HsDecl GhcRn, DocForDecl Name)]
    findBundledPatterns avail = do
      patsyns <- for constructor_names $ \name -> do
        mtyThing <- lookupName name
        case mtyThing of
          Just (AConLike PatSynCon{}) -> do
            export_items <- declWith (Avail.avail name)
            pure [ (unLoc patsyn_decl, patsyn_doc)
                 | ExportDecl {
                       expItemDecl  = patsyn_decl
                     , expItemMbDoc = patsyn_doc
                     } <- export_items
                 ]
          _ -> pure []
      pure (concat patsyns)
      where
        constructor_names =
          filter isDataConName (availSubordinates avail)

availSubordinates :: AvailInfo -> [Name]
availSubordinates = map greNameMangledName . availSubordinateGreNames

availNoDocs :: AvailInfo -> [(Name, DocForDecl Name)]
availNoDocs avail =
  zip (availSubordinates avail) (repeat noDocForDecl)

hiDecl :: Monad m => DynFlags -> PrintRuntimeReps -> Name -> IfM m (Maybe (LHsDecl GhcRn))
hiDecl dflags prr t = do
  mayTyThing <- lookupName t
  case mayTyThing of
    Nothing -> do
      liftErrMsg $ tell ["Warning: Not found in environment: " ++ pretty dflags t]
      return Nothing
    Just x -> case tyThingToLHsDecl prr x of
      Left m -> liftErrMsg (tell [bugWarn m]) >> return Nothing
      Right (m, t') -> liftErrMsg (tell $ map bugWarn m)
                      >> return (Just $ L (noAnnSrcSpan (nameSrcSpan t)) t')
    where
      warnLine x = O.text "haddock-bug:" O.<+> O.text x O.<>
                   O.comma O.<+> O.quotes (O.ppr t) O.<+>
                   O.text "-- Please report this on Haddock issue tracker!"
      bugWarn = showSDoc dflags . warnLine

-- | Lookup docs for a declaration from maps.
lookupDocs :: AvailInfo -> WarningMap -> Map Name (MDoc Name) -> ArgMap Name -> OccEnv Name
           -> (DocForDecl Name, [(Name, DocForDecl Name)])
lookupDocs avail warnings docMap argMap def_meths_env =
  let n = availName avail in
  let lookupArgDoc x = M.findWithDefault M.empty x argMap in
  let doc = (lookupDoc n, lookupArgDoc n)
      subs = availSubordinates avail
      def_meths = [ (meth, (lookupDoc meth, lookupArgDoc meth))
                  | s <- subs
                  , let dmOcc = mkDefaultMethodOcc (nameOccName s)
                  , Just meth <- [lookupOccEnv def_meths_env dmOcc]] in
  let subDocs = [ (s, (lookupDoc s, lookupArgDoc s))
                | s <- subs
                ] ++ def_meths in
  (doc, subDocs)
  where
    lookupDoc name = Documentation (M.lookup name docMap) (M.lookup name warnings)


-- Note [1]:
------------
-- It is unnecessary to document a subordinate by itself at the top level if
-- any of its parents is also documented. Furthermore, if the subordinate is a
-- record field or a class method, documenting it under its parent
-- indicates its special status.
--
-- A user might expect that it should show up separately, so we issue a
-- warning. It's a fine opportunity to also tell the user she might want to
-- export the subordinate through the parent export item for clarity.
--
-- The code removes top-level subordinates also when the parent is exported
-- through a 'module' export. I think that is fine.
--
-- (For more information, see Trac #69)


-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble
-- together a type signature for it...).
--
-- This function looks through the declarations in this module to try to find
-- the one with the right name.
extractDecl
  :: Monad m
  => PrintRuntimeReps
  -> DynFlags
  -> Name                      -- ^ name of the declaration to extract
  -> LHsDecl GhcRn             -- ^ parent declaration
  -> IfM m (Either ErrMsg (LHsDecl GhcRn))
extractDecl prr dflags name decl
  | name `elem` getMainDeclBinder emptyOccEnv (unLoc decl) = pure $ Right decl
  | otherwise  =
    case unLoc decl of
      TyClD _ d@ClassDecl { tcdLName = L _ clsNm
                          , tcdSigs = clsSigs
                          , tcdATs = clsATs } ->
        let
          matchesMethod =
            [ lsig
            | lsig <- clsSigs
            , ClassOpSig _ False _ _ <- pure $ unLoc lsig
              -- Note: exclude `default` declarations (see #505)
            , name `elem` sigName lsig
            ]

          matchesAssociatedType =
            [ lfam_decl
            | lfam_decl <- clsATs
            , name == unLoc (fdLName (unLoc lfam_decl))
            ]

            -- TODO: document fixity
        in case (matchesMethod, matchesAssociatedType)  of
          ([s0], _) -> let tyvar_names = tyClDeclTyVars d
                           L pos sig = addClassContext clsNm tyvar_names s0
                       in pure (Right $ L pos (SigD noExtField sig))
          (_, [L pos fam_decl]) -> pure (Right $ L pos (TyClD noExtField (FamDecl noExtField fam_decl)))

          ([], []) -> do
            famInstDeclOpt <- hiDecl dflags prr name
            case famInstDeclOpt of
              Nothing -> pure $ Left (concat [ "Ambiguous decl for ", getOccString name
                            , " in class ", getOccString clsNm ])
              Just famInstDecl -> extractDecl prr dflags name famInstDecl
          _ -> pure $ Left (concat [ "Ambiguous decl for ", getOccString name
                            , " in class ", getOccString clsNm ])
      TyClD _ d@DataDecl { tcdLName = L _ dataNm
                         , tcdDataDefn = HsDataDefn { dd_cons = dataCons } } -> pure $ do
        let ty_args = lHsQTyVarsToTypes (tyClDeclTyVars d)
        lsig <- if isDataConName name
                  then extractPatternSyn name dataNm ty_args dataCons
                  else extractRecSel name dataNm ty_args dataCons
        pure (SigD noExtField <$> lsig)

      TyClD _ FamDecl {}
        | isValName name -> do
            famInstOpt <- hiDecl dflags prr name
            case famInstOpt of
              Just famInst -> extractDecl prr dflags name famInst
              Nothing -> pure $ Left ("extractDecl: Unhandled decl for " ++ getOccString name)

      InstD _ (DataFamInstD _ (DataFamInstDecl
                            (FamEqn { feqn_tycon = L _ n
                                    , feqn_pats  = tys
                                    , feqn_rhs   = defn }))) -> pure $
        if isDataConName name
        then fmap (SigD noExtField) <$> extractPatternSyn name n tys (dd_cons defn)
        else fmap (SigD noExtField) <$> extractRecSel name n tys (dd_cons defn)
      InstD _ (ClsInstD _ ClsInstDecl { cid_datafam_insts = insts })
        | isDataConName name ->
            let matches = [ d' | L _ d'@(DataFamInstDecl (FamEqn { feqn_rhs = dd })) <- insts
                               , name `elem` map unLoc (concatMap (getConNames . unLoc) (dd_cons dd))
                               ]
            in case matches of
                [d0] -> extractDecl prr dflags name (noLocA (InstD noExtField (DataFamInstD noExtField d0)))
                _    -> pure $ Left "internal: extractDecl (ClsInstD)"
        | otherwise ->
            let matches = [ d' | L _ d'@(DataFamInstDecl d )
                                   <- insts
                                 -- , L _ ConDecl { con_details = RecCon rec } <- dd_cons (feqn_rhs d)
                               , Just rec <- map (getRecConArgs_maybe . unLoc) (dd_cons (feqn_rhs d))
                               , ConDeclField { cd_fld_names = ns } <- map unLoc (unLoc rec)
                               , L _ n <- ns
                               , foExt n == name
                          ]
            in case matches of
              [d0] -> extractDecl prr dflags name (noLocA . InstD noExtField $ DataFamInstD noExtField d0)
              _ -> pure $ Left "internal: extractDecl (ClsInstD)"
      _ -> pure $ Left ("extractDecl: Unhandled decl for " ++ getOccString name)

extractPatternSyn :: Name -> Name
                  -> [LHsTypeArg GhcRn] -> [LConDecl GhcRn]
                  -> Either ErrMsg (LSig GhcRn)
extractPatternSyn nm t tvs cons =
  case filter matches cons of
    [] -> Left . O.showSDocOneLine O.defaultSDocContext $
          O.text "constructor pattern " O.<+> O.ppr nm O.<+> O.text "not found in type" O.<+> O.ppr t
    con:_ -> pure (extract <$> con)
 where
  matches :: LConDecl GhcRn -> Bool
  matches (L _ con) = nm `elem` (unLoc <$> getConNames con)
  extract :: ConDecl GhcRn -> Sig GhcRn
  extract con =
    let args =
          case con of
            ConDeclH98 { con_args = con_args' } -> case con_args' of
              PrefixCon _ args' -> map hsScaledThing args'
              RecCon (L _ fields) -> cd_fld_type . unLoc <$> fields
              InfixCon arg1 arg2 -> map hsScaledThing [arg1, arg2]
            ConDeclGADT { con_g_args = con_args' } -> case con_args' of
              PrefixConGADT args' -> map hsScaledThing args'
              RecConGADT (L _ fields) _ -> cd_fld_type . unLoc <$> fields
        typ = longArrow args (data_ty con)
        typ' =
          case con of
            ConDeclH98 { con_mb_cxt = Just cxt } -> noLocA (HsQualTy noExtField cxt typ)
            _ -> typ
        typ'' = noLocA (HsQualTy noExtField (noLocA []) typ')
    in PatSynSig noAnn [noLocA nm] (mkEmptySigType typ'')

  longArrow :: [LHsType GhcRn] -> LHsType GhcRn -> LHsType GhcRn
  longArrow inputs output = foldr (\x y -> noLocA (HsFunTy noAnn (HsUnrestrictedArrow noHsUniTok) x y)) output inputs

  data_ty con
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise = foldl' (\x y -> noLocA (mkAppTyArg x y)) (noLocA (HsTyVar noAnn NotPromoted (noLocA t))) tvs
                    where mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
                          mkAppTyArg f (HsValArg ty) = HsAppTy noExtField f ty
                          mkAppTyArg f (HsTypeArg l ki) = HsAppKindTy l f ki
                          mkAppTyArg f (HsArgPar _) = HsParTy noAnn f

extractRecSel :: Name -> Name -> [LHsTypeArg GhcRn] -> [LConDecl GhcRn]
              -> Either ErrMsg (LSig GhcRn)
extractRecSel _ _ _ [] = Left "extractRecSel: selector not found"

extractRecSel nm t tvs (L _ con : rest) =
  case getRecConArgs_maybe con of
    Just (L _ fields) | ((l,L _ (ConDeclField _ _nn ty _)) : _) <- matching_fields fields ->
      pure (L (noAnnSrcSpan l) (TypeSig noAnn [noLocA nm] (mkEmptyWildCardBndrs $ mkEmptySigType (noLocA (HsFunTy noAnn (HsUnrestrictedArrow noHsUniTok) data_ty (getBangType ty))))))
    _ -> extractRecSel nm t tvs rest
 where
  matching_fields :: [LConDeclField GhcRn] -> [(SrcSpan, LConDeclField GhcRn)]
  matching_fields flds = [ (locA l,f) | f@(L _ (ConDeclField _ ns _ _)) <- flds
                                      , L l n <- ns, foExt n == nm ]
  data_ty
    -- ResTyGADT _ ty <- con_res con = ty
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise = foldl' (\x y -> noLocA (mkAppTyArg x y)) (noLocA (HsTyVar noAnn NotPromoted (noLocA t))) tvs
                   where mkAppTyArg :: LHsType GhcRn -> LHsTypeArg GhcRn -> HsType GhcRn
                         mkAppTyArg f (HsValArg ty) = HsAppTy noExtField f ty
                         mkAppTyArg f (HsTypeArg l ki) = HsAppKindTy l f ki
                         mkAppTyArg f (HsArgPar _) = HsParTy noAnn f

-- | Keep export items with docs.
pruneExportItems :: [ExportItem GhcRn] -> [ExportItem GhcRn]
pruneExportItems = filter hasDoc
  where
    hasDoc (ExportDecl{expItemMbDoc = (Documentation d _, _)}) = isJust d
    hasDoc _ = True


mkVisibleNames :: InstMap -> [ExportItem GhcRn] -> [DocOption] -> [Name]
mkVisibleNames instMap exports opts
  | OptHide `elem` opts = []
  | otherwise = let ns = concatMap exportName exports
                in seqList ns `seq` ns
  where
    exportName e@ExportDecl {} = name ++ subs ++ patsyns
      where subs    = map fst (expItemSubDocs e)
            patsyns = concatMap (getMainDeclBinder emptyOccEnv . fst) (expItemPats e)
            name = case unLoc $ expItemDecl e of
              InstD _ d -> maybeToList $ SrcLoc.lookupSrcSpan (getInstLoc d) instMap
              decl      -> getMainDeclBinder emptyOccEnv decl
    exportName ExportNoDecl {} = [] -- we don't count these as visible, since
                                    -- we don't want links to go to them.
    exportName _ = []

seqList :: [a] -> ()
seqList [] = ()
seqList (x : xs) = x `seq` seqList xs
