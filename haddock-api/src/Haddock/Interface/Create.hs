{-# LANGUAGE CPP, TupleSections, BangPatterns, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
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
module Haddock.Interface.Create (createInterface) where

import Documentation.Haddock.Doc (metaDocAppend)
import Documentation.Haddock.Utf8 as Utf8
import Haddock.Types
import Haddock.Options
import Haddock.GhcUtils
import Haddock.Utils
import Haddock.Convert
import Haddock.Interface.LexParseRn
import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Ast as Hyperlinker
import Haddock.Backends.Hyperlinker.Parser as Hyperlinker

import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Ord
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Exception (evaluate)
import Control.Monad
import Data.Traversable

import Avail hiding (avail)
import qualified Avail
import qualified Module
import qualified SrcLoc
import ConLike (ConLike(..))
import GHC
import GhcMonad
import HscTypes
import Name
import NameSet
import NameEnv
import qualified Outputable
import Packages ( PackageName(..) )
import Bag
import SrcLoc
import TcIface
import TcRnMonad
import FastString ( unpackFS )
import HsDecls ( getConArgs )
import BasicTypes ( SourceText(..), WarningTxt(..), WarningSort(..), warningTxtContents )
import qualified Outputable as O
import DynFlags ( getDynFlags )

-- | Use a 'ModIface' to produce an 'Interface'.
-- To do this, we need access to already processed modules in the topological
-- sort. That's what's in the 'IfaceMap'.
createInterface :: ModIface
                -> [Flag]       -- Boolean flags
                -> IfaceMap     -- Locally processed modules
                -> InstIfaceMap -- External, already installed interfaces
                -> ErrMsgGhc Interface
createInterface mod_iface flags modMap instIfaceMap = do
  dflags <- getDynFlags

  let mdl            = mi_module mod_iface
      sem_mdl        = mi_semantic_module mod_iface
      is_sig         = isJust (mi_sig_of mod_iface)
      safety         = getSafeMode (mi_trust mod_iface)

      -- Not sure whether the relevant info is in these dflags
      (pkgNameFS, _) = modulePackageInfo dflags flags mdl
      pkgName        = fmap (unpackFS . (\(PackageName n) -> n)) pkgNameFS
      warnings       = mi_warns mod_iface
      !exportedNames = concatMap availNamesWithSelectors (mi_exports mod_iface)
      fixMap         = mkFixMap exportedNames (mi_fixities mod_iface)

  mod_iface_docs <- case mi_docs mod_iface of
    Just docs -> pure docs
    Nothing -> do
      liftErrMsg $ tell [O.showPpr dflags mdl ++ " has no docs in its .hi-file"]
      pure emptyDocs

  let renamer = docIdEnvRenamer (docs_id_env mod_iface_docs)

  opts <- liftErrMsg $ mkDocOpts (docs_haddock_opts mod_iface_docs) flags mdl

  -- Process the top-level module header documentation.
  (!info, mbDoc) <- processModuleHeader pkgName renamer safety
                                        (docs_language mod_iface_docs)
                                        (docs_extensions mod_iface_docs)
                                        (hsDocString <$> docs_mod_hdr mod_iface_docs)

  modWarn <- moduleWarning renamer (hsDocString <$> warnings)

  let process = processDocStringParas pkgName renamer . hsDocString
  docMap <- traverse process (docs_decls mod_iface_docs)
  argMap <- traverse (traverse process) (docs_args mod_iface_docs)

  warningMap <- mkWarningMap (hsDocString <$> warnings) renamer exportedNames

  mod_details <- liftGhcToErrMsgGhc $ withSession $ \hsc_env -> do
    liftIO $ initIfaceCheck (Outputable.text "createInterface'") hsc_env (typecheckIface mod_iface)

  -- Are these all the (fam_)instances that we need?
  let instances = md_insts mod_details
      fam_instances = md_fam_insts mod_details

  -- TODO: Entirely remove DeclMap.
  let declMap = M.empty

  let localInsts = filter (nameIsLocalOrFrom sem_mdl)
                        $  map getName instances
                        ++ map getName fam_instances
      instanceMap = M.fromList (map (getSrcSpan &&& id) localInsts)

  let maps = (docMap, argMap, declMap, instanceMap)
      allWarnings = M.unions (warningMap : map ifaceWarningMap (M.elems modMap))

      -- Locations of all TH splices
      -- TODO: We use the splice info in 'Haddock.Backends.Xhtml.Layout.links' to
      -- determine what kind of link we want to generate. Since we depend on
      -- declaration locations there, it makes sense to get the splice locations
      -- together with the other locations from the extended .hie files.
      splices = []

  exportItems <- mkExportItems (docs_structure mod_iface_docs)
                               (docs_named_chunks mod_iface_docs)
                               is_sig modMap pkgName mdl sem_mdl allWarnings
                               renamer exportedNames maps fixMap
                               splices instIfaceMap

  let !visibleNames = mkVisibleNames maps exportItems opts

  -- Measure haddock documentation coverage.
  let prunedExportItems0 = pruneExportItems exportItems
      !haddockable = 1 + length exportItems -- module + exports
      !haddocked = (if isJust mbDoc then 1 else 0) + length prunedExportItems0
      !coverage = (haddockable, haddocked)

  -- Prune the export list to just those declarations that have
  -- documentation, if the 'prune' option is on.
  let prunedExportItems'
        | OptPrune `elem` opts = prunedExportItems0
        | otherwise = exportItems
      !prunedExportItems = seqList prunedExportItems' `seq` prunedExportItems'

  return $! Interface {
    ifaceMod               = mdl
  , ifaceIsSig             = is_sig
  , ifaceInfo              = info
  , ifaceDoc               = Documentation mbDoc modWarn
  , ifaceRnDoc             = Documentation Nothing Nothing
  , ifaceOptions           = opts
  , ifaceDocMap            = docMap
  , ifaceArgMap            = argMap
  , ifaceRnDocMap          = M.empty
  , ifaceRnArgMap          = M.empty
  , ifaceExportItems       = prunedExportItems
  , ifaceRnExportItems     = []
  , ifaceExports           = exportedNames
  , ifaceVisibleExports    = visibleNames
  , ifaceDeclMap           = declMap
  , ifaceFixMap            = fixMap
  , ifaceInstances         = instances
  , ifaceFamInstances      = fam_instances
  , ifaceOrphanInstances   = []
  , ifaceRnOrphanInstances = []
  , ifaceHaddockCoverage   = coverage
  , ifaceWarningMap        = warningMap
  , ifaceTokenizedSrc      = Nothing -- Ignore for now.
  }

-- TODO: Do we need a special case for the current module?
unrestrictedModExports :: Avails -> [ModuleName]
                      -> ErrMsgGhc ([Module], Avails)
                      -- ^ ( modules exported without restriction
                      --   , remaining exports not included in any
                      --     of these modules
                      --   )
unrestrictedModExports avails mod_names = do
    let all_names = availsToNameSetWithSelectors avails
    mods_and_exports <- fmap catMaybes $ for mod_names $ \mod_name -> do
      mdl <- liftGhcToErrMsgGhc $ findModule mod_name Nothing
      mb_modinfo <- liftGhcToErrMsgGhc $ getModuleInfo mdl
      case mb_modinfo of
        Nothing -> do
          dflags <- getDynFlags
          liftErrMsg $ tell [ "Bug: unrestrictedModExports: " ++ pretty dflags mdl]
          pure Nothing
        Just modinfo ->
          pure (Just (mdl, mkNameSet (modInfoExportsWithSelectors modinfo)))
    let unrestricted = filter (\(_, exps) -> exps `isSubsetOf` all_names) mods_and_exports
        mod_exps = unionNameSets (map snd unrestricted)
        remaining = nubAvails (filterAvails (\n -> not (n `elemNameSet` mod_exps)) avails)
    pure (map fst unrestricted, remaining)
  where
    -- TODO: Add a utility based on IntMap.isSubmapOfBy
    isSubsetOf :: NameSet -> NameSet -> Bool
    isSubsetOf a b = nameSetAll (`elemNameSet` b) a


-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

-- TODO: Either find a different way of looking up the OccNames or change the Warnings or
-- WarningMap type.
mkWarningMap :: Warnings HsDocString -> Renamer -> [Name] -> ErrMsgGhc WarningMap
mkWarningMap warnings renamer exps = case warnings of
  NoWarnings  -> pure M.empty
  WarnAll _   -> pure M.empty
  WarnSome ws ->
    -- Not sure if this is equivalent to the original code below.
    let expsOccEnv = mkOccEnv [(nameOccName n, n) | n <- exps]
        ws' = flip mapMaybe ws $ \(occ, w) ->
                (,w) <$> lookupOccEnv expsOccEnv occ
    {-
    let ws' = [ (n, w)
              | (occ, w) <- ws
              , elt <- lookupGlobalRdrEnv gre occ
              , let n = gre_name elt, n `elem` exps ]
    -}
    in M.fromList <$> traverse (traverse (parseWarning renamer)) ws'

moduleWarning :: Renamer -> Warnings HsDocString -> ErrMsgGhc (Maybe (Doc Name))
moduleWarning _ NoWarnings = pure Nothing
moduleWarning _ (WarnSome _) = pure Nothing
moduleWarning renamer (WarnAll w) = Just <$> parseWarning renamer w

parseWarning :: Renamer -> WarningTxt HsDocString -> ErrMsgGhc (Doc Name)
parseWarning renamer w =
  format heading (foldl' appendHDSAsParagraphs (mkHsDocString "") msgs)
  where
    format x msg = DocWarning . DocParagraph . DocAppend (DocString x)
                   <$> processDocString renamer msg
    heading = case sort_ of
      WsWarning -> "Warning: "
      WsDeprecated -> "Deprected: "
    (sort_, msgs) = warningTxtContents w


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
parseOption other = tell ["Unrecognised option: " ++ other] >> return Nothing

type Maps = (DocMap Name, ArgMap Name, DeclMap, InstMap)

-- | Extract a map of fixity declarations only
mkFixMap :: [Name] -> [(OccName, Fixity)] -> FixMap
mkFixMap exps occFixs =
    M.fromList $ flip mapMaybe occFixs $ \(occ, fix_) ->
      (,fix_) <$> lookupOccEnv expsOccEnv occ
  where
    expsOccEnv = mkOccEnv (map (nameOccName &&& id) exps)

mkExportItems
  :: DocStructure
  -> Map String HsDoc' -- Named chunks
  -> Bool               -- is it a signature
  -> IfaceMap
  -> Maybe Package      -- this package
  -> Module             -- this module
  -> Module             -- semantic module
  -> WarningMap
  -> Renamer
  -> [Name]             -- exported names (orig)
--  -> [LHsDecl GhcRn]    -- renamed source declarations
  -> Maps
  -> FixMap
  -> [RealSrcSpan]        -- splice locations
--  -> Avails             -- exported stuff from this module
  -> InstIfaceMap
  -> ErrMsgGhc [ExportItem GhcRn]
mkExportItems dsItems namedChunks is_sig ifaceMap mbPkgName thisMod semMod warnings renamer exportedNames maps fixMap splices instIfaceMap = do
    concat <$> traverse lookupExport dsItems
  where
    lookupExport :: DocStructureItem -> ErrMsgGhc [ExportItem GhcRn]
    lookupExport = \case
      DsiSectionHeading lev hsDoc' -> do
        doc <- processDocString renamer (hsDocString hsDoc')
        pure [ExportGroup lev "" doc]
      DsiDocChunk hsDoc' -> do
        doc <- processDocStringParas mbPkgName renamer (hsDocString hsDoc')
        pure [ExportDoc doc]
      DsiNamedChunkRef ref -> do
        case M.lookup ref namedChunks of
          Nothing -> do
            liftErrMsg $ tell ["Cannot find documentation for: $" ++ ref]
            pure []
          Just hsDoc' -> do
            doc <- processDocStringParas mbPkgName renamer (hsDocString hsDoc')
            pure [ExportDoc doc]
      DsiExports avails ->
        -- TODO: We probably don't need nubAvails here.
        -- mkDocStructureFromExportList already uses it.
        concat <$> traverse availExport (nubAvails avails)
      DsiModExport mod_names avails -> do
        -- only consider exporting a module if we are sure we
        -- are really exporting the whole module and not some
        -- subset.
        (unrestricted_mods, remaining_avails) <- unrestrictedModExports avails (NE.toList mod_names)
        avail_exps <- concat <$> traverse availExport remaining_avails
        pure (map ExportModule unrestricted_mods ++ avail_exps)

    availExport avail =
      availExportItem is_sig ifaceMap thisMod semMod warnings exportedNames
        maps fixMap splices instIfaceMap avail

availExportItem :: Bool               -- is it a signature
                -> IfaceMap
                -> Module             -- this module
                -> Module             -- semantic module
                -> WarningMap
                -> [Name]             -- exported names (orig)
                -> Maps
                -> FixMap
                -> [RealSrcSpan]      -- splice locations
                -> InstIfaceMap
                -> AvailInfo
                -> ErrMsgGhc [ExportItem GhcRn]
availExportItem is_sig modMap thisMod semMod warnings exportedNames
  (docMap, argMap, declMap, _) fixMap splices instIfaceMap
  availInfo = declWith availInfo
  where
    declWith :: AvailInfo -> ErrMsgGhc [ ExportItem GhcRn ]
    declWith avail = do
      dflags <- getDynFlags
      let t = availName avail -- 't' may not be in the scope of 'avail'.
                              -- Example: @data C = D@, where C isn't exported.
      r    <- findDecl avail
      case r of
        ([L l (ValD _ _)], (doc, _)) -> do
          -- Top-level binding without type signature
          export <- hiValExportItem t l doc (isSplice l) $ M.lookup t fixMap
          return [export]
        (ds, docs_) | decl : _ <- filter (not . isValD . unLoc) ds ->
          let declNames = getMainDeclBinder (unL decl)
          in case () of
            _
              -- We should not show a subordinate by itself if any of its
              -- parents is also exported. See note [1].
              | t `notElem` declNames,
                Just p <- find isExported (parents t $ unL decl) ->
                do liftErrMsg $ tell [
                     "Warning: " ++ moduleString thisMod ++ ": " ++
                     pretty dflags (nameOccName t) ++ " is exported separately but " ++
                     "will be documented under " ++ pretty dflags (nameOccName p) ++
                     ". Consider exporting it together with its parent(s)" ++
                     " for code clarity." ]
                   return []

              -- normal case
              | otherwise -> case decl of
                  -- A single signature might refer to many names, but we
                  -- create an export item for a single name only.  So we
                  -- modify the signature to contain only that single name.
                  L loc (SigD _ sig) ->
                    -- fromJust is safe since we already checked in guards
                    -- that 't' is a name declared in this declaration.
                    -- That's wrong.
                    case filterSigNames (== t) sig of
                      Nothing -> do
                        liftErrMsg $ tell [
                          "Warning: " ++ moduleString thisMod ++ ": " ++
                          pretty dflags sig ++ " doesn't contain " ++ pretty dflags t ++
                          ". Names in the signature: " ++ pretty dflags (sigNameNoLoc sig)]
                        pure []
                      Just sig' ->
                        availExportDecl avail (L loc (SigD noExt sig'))  docs_
                  L loc (TyClD _ cl@ClassDecl{}) -> do
                    mdef <- liftGhcToErrMsgGhc $ minimalDef t
                    let sig = maybeToList $ fmap (noLoc . MinimalSig noExt NoSourceText . noLoc . fmap noLoc) mdef
                    availExportDecl avail
                      (L loc $ TyClD noExt cl { tcdSigs = sig ++ tcdSigs cl }) docs_

                  _ -> availExportDecl avail decl docs_

        ([], _) -> do
          mayDecl <- hiDecl t
          case mayDecl of
            Nothing -> return [ ExportNoDecl t [] ]
            Just decl -> do
              docs_ <- do
                let tmod = nameModule t
                if tmod == thisMod
                  then pure (lookupDocs avail warnings docMap argMap)
                  else case M.lookup tmod modMap of
                    Just iface ->
                      pure (lookupDocs avail warnings (ifaceDocMap iface) (ifaceArgMap iface))
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
                          pure (lookupDocs avail warnings (instDocMap instIface) (instArgMap instIface))
              availExportDecl avail decl docs_

        _ -> return []

    availExportDecl :: AvailInfo -> LHsDecl GhcRn
                    -> (DocForDecl Name, [(Name, DocForDecl Name)])
                    -> ErrMsgGhc [ ExportItem GhcRn ]
    availExportDecl avail decl (doc, subs)
      | availExportsDecl avail = do
          -- bundled pattern synonyms only make sense if the declaration is
          -- exported (otherwise there would be nothing to bundle to)
          bundledPatSyns <- findBundledPatterns avail

          let
            patSynNames =
              concatMap (getMainDeclBinder . fst) bundledPatSyns

            fixities =
                [ (n, f)
                | n <- availName avail : fmap fst subs ++ patSynNames
                , Just f <- [M.lookup n fixMap]
                ]

          return [ ExportDecl {
                       expItemDecl      = restrictTo (fmap fst subs)
                                            (extractDecl declMap (availName avail) decl)
                     , expItemPats      = bundledPatSyns
                     , expItemMbDoc     = doc
                     , expItemSubDocs   = subs
                     , expItemInstances = []
                     , expItemFixities  = fixities
                     , expItemSpliced   = False
                     }
                 ]

      | otherwise =
          return [ ExportDecl {
                       expItemDecl      = extractDecl declMap sub decl
                     , expItemPats      = []
                     , expItemMbDoc     = sub_doc
                     , expItemSubDocs   = []
                     , expItemInstances = []
                     , expItemFixities  = [ (sub, f) | Just f <- [M.lookup sub fixMap] ]
                     , expItemSpliced   = False
                     }
                 | (sub, sub_doc) <- subs
                 ]

    exportedNameSet = mkNameSet exportedNames
    isExported n = elemNameSet n exportedNameSet

    findDecl :: AvailInfo -> ErrMsgGhc ([LHsDecl GhcRn], (DocForDecl Name, [(Name, DocForDecl Name)]))
    findDecl avail
      | m == semMod =
          case M.lookup n declMap of
            Just ds -> return (ds, lookupDocs avail warnings docMap argMap)
            Nothing
              | is_sig -> do
                -- OK, so it wasn't in the local declaration map.  It could
                -- have been inherited from a signature.  Reconstitute it
                -- from the type.
                mb_r <- hiDecl n
                case mb_r of
                    Nothing -> return ([], (noDocForDecl, availNoDocs avail))
                    -- TODO: If we try harder, we might be able to find
                    -- a Haddock!  Look in the Haddocks for each thing in
                    -- requirementContext (pkgState)
                    Just decl -> return ([decl], (noDocForDecl, availNoDocs avail))
              | otherwise ->
                return ([], (noDocForDecl, availNoDocs avail))
      | Just iface <- M.lookup (semToIdMod (moduleUnitId thisMod) m) modMap
      , Just ds <- M.lookup n (ifaceDeclMap iface) =
          return (ds, lookupDocs avail warnings
                            (ifaceDocMap iface)
                            (ifaceArgMap iface))
      | otherwise = return ([], (noDocForDecl, availNoDocs avail))
      where
        n = availName avail
        m = nameModule n

    findBundledPatterns :: AvailInfo -> ErrMsgGhc [(HsDecl GhcRn, DocForDecl Name)]
    findBundledPatterns avail = do
      patsyns <- for constructor_names $ \name -> do
        mtyThing <- liftGhcToErrMsgGhc (lookupName name)
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

    isSplice :: SrcSpan -> Bool
    isSplice (RealSrcSpan rss0) = any (\rss -> rss `containsSpan` rss0) splices
    isSplice UnhelpfulSpan {} = False

-- this heavily depends on the invariants stated in Avail
availExportsDecl :: AvailInfo -> Bool
availExportsDecl (AvailTC ty_name names _)
  | n : _ <- names = ty_name == n
  | otherwise      = False
availExportsDecl _ = True

availSubordinates :: AvailInfo -> [Name]
availSubordinates avail =
  filter (/= availName avail) (availNamesWithSelectors avail)

availNoDocs :: AvailInfo -> [(Name, DocForDecl Name)]
availNoDocs avail =
  zip (availSubordinates avail) (repeat noDocForDecl)

-- | Given a 'Module' from a 'Name', convert it into a 'Module' that
-- we can actually find in the 'IfaceMap'.
semToIdMod :: UnitId -> Module -> Module
semToIdMod this_uid m
    | Module.isHoleModule m = mkModule this_uid (moduleName m)
    | otherwise      = m

hiDecl :: Name -> ErrMsgGhc (Maybe (LHsDecl GhcRn))
hiDecl t = do
  dflags <- getDynFlags
  mayTyThing <- liftGhcToErrMsgGhc $ lookupName t
  let bugWarn = O.showSDoc dflags . warnLine
  case mayTyThing of
    Nothing -> do
      liftErrMsg $ tell ["Warning: Not found in environment: " ++ pretty dflags t]
      return Nothing
    Just x -> case tyThingToLHsDecl x of
      Left m -> liftErrMsg (tell [bugWarn m]) >> return Nothing
      Right (m, t') -> liftErrMsg (tell $ map bugWarn m)
                      >> return (Just $ noLoc t')
    where
      warnLine x = O.text "haddock-bug:" O.<+> O.text x O.<>
                   O.comma O.<+> O.quotes (O.ppr t) O.<+>
                   O.text "-- Please report this on Haddock issue tracker!"

-- | This function is called for top-level bindings without type signatures.
-- It gets the type signature from GHC and that means it's not going to
-- have a meaningful 'SrcSpan'. So we pass down 'SrcSpan' for the
-- declaration and use it instead - 'nLoc' here.
hiValExportItem :: Name -> SrcSpan -> DocForDecl Name -> Bool
                -> Maybe Fixity -> ErrMsgGhc (ExportItem GhcRn)
hiValExportItem name nLoc doc splice fixity = do
  mayDecl <- hiDecl name
  case mayDecl of
    Nothing -> return (ExportNoDecl name [])
    Just decl -> return (ExportDecl (fixSpan decl) [] doc [] [] fixities splice)
  where
    fixSpan (L l t) = L (SrcLoc.combineSrcSpans l nLoc) t
    fixities = case fixity of
      Just f  -> [(name, f)]
      Nothing -> []


-- | Lookup docs for a declaration from maps.
lookupDocs :: AvailInfo -> WarningMap -> DocMap Name -> ArgMap Name
           -> (DocForDecl Name, [(Name, DocForDecl Name)])
lookupDocs avail warnings docMap argMap =
  let n = availName avail in
  let lookupArgDoc x = M.findWithDefault M.empty x argMap in
  let doc = (lookupDoc n, lookupArgDoc n) in
  let subDocs = [ (s, (lookupDoc s, lookupArgDoc s))
                | s <- availSubordinates avail
                ] in
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
extractDecl :: DeclMap -> Name -> LHsDecl GhcRn -> LHsDecl GhcRn
extractDecl declMap name decl
  | name `elem` getMainDeclBinder (unLoc decl) = decl
  | otherwise  =
    case unLoc decl of
      TyClD _ d@ClassDecl {} ->
        let
          matchesMethod =
            [ lsig
            | lsig <- tcdSigs d
            , ClassOpSig _ False _ _ <- pure $ unLoc lsig
              -- Note: exclude `default` declarations (see #505)
            , name `elem` sigName lsig
            ]

          matchesAssociatedType =
            [ lfam_decl
            | lfam_decl <- tcdATs d
            , name == unLoc (fdLName (unLoc lfam_decl))
            ]

            -- TODO: document fixity
        in case (matchesMethod, matchesAssociatedType)  of
          ([s0], _) -> let (n, tyvar_names) = (tcdName d, tyClDeclTyVars d)
                           L pos sig = addClassContext n tyvar_names s0
                       in L pos (SigD noExt sig)
          (_, [L pos fam_decl]) -> L pos (TyClD noExt (FamDecl noExt fam_decl))

          ([], [])
            | Just (famInstDecl:_) <- M.lookup name declMap
            -> extractDecl declMap name famInstDecl
          _ -> O.pprPanic "extractDecl" (O.text "Ambiguous decl for" O.<+> O.ppr name O.<+> O.text "in class:"
                                         O.$$ O.nest 4 (O.ppr d)
                                         O.$$ O.text "Matches:"
                                         O.$$ O.nest 4 (O.ppr matchesMethod O.<+> O.ppr matchesAssociatedType))
      TyClD _ d@DataDecl {} ->
        let (n, tyvar_tys) = (tcdName d, lHsQTyVarsToTypes (tyClDeclTyVars d))
        in if isDataConName name
           then SigD noExt <$> extractPatternSyn name n tyvar_tys (dd_cons (tcdDataDefn d))
           else SigD noExt <$> extractRecSel name n tyvar_tys (dd_cons (tcdDataDefn d))
      TyClD _ FamDecl {}
        | isValName name
        , Just (famInst:_) <- M.lookup name declMap
        -> extractDecl declMap name famInst
      InstD _ (DataFamInstD _ (DataFamInstDecl (HsIB { hsib_body =
                             FamEqn { feqn_tycon = L _ n
                                    , feqn_pats  = tys
                                    , feqn_rhs   = defn }}))) ->
        if isDataConName name
        then SigD noExt <$> extractPatternSyn name n tys (dd_cons defn)
        else SigD noExt <$> extractRecSel name n tys (dd_cons defn)
      InstD _ (ClsInstD _ ClsInstDecl { cid_datafam_insts = insts })
        | isDataConName name ->
            let matches = [ d' | L _ d'@(DataFamInstDecl (HsIB { hsib_body =
                                          FamEqn { feqn_rhs   = dd
                                                 }
                                         })) <- insts
                               , name `elem` map unLoc (concatMap (getConNames . unLoc) (dd_cons dd))
                               ]
            in case matches of
                [d0] -> extractDecl declMap name (noLoc (InstD noExt (DataFamInstD noExt d0)))
                _    -> error "internal: extractDecl (ClsInstD)"
        | otherwise ->
            let matches = [ d' | L _ d'@(DataFamInstDecl (HsIB { hsib_body = d }))
                                   <- insts
                                 -- , L _ ConDecl { con_details = RecCon rec } <- dd_cons (feqn_rhs d)
                               , RecCon rec <- map (getConArgs . unLoc) (dd_cons (feqn_rhs d))
                               , ConDeclField { cd_fld_names = ns } <- map unLoc (unLoc rec)
                               , L _ n <- ns
                               , extFieldOcc n == name
                          ]
            in case matches of
              [d0] -> extractDecl declMap name (noLoc . InstD noExt $ DataFamInstD noExt d0)
              _ -> error "internal: extractDecl (ClsInstD)"
      x -> O.pprPanic "extractDecl" (O.ppr x)


extractPatternSyn :: Name -> Name -> [LHsType GhcRn] -> [LConDecl GhcRn] -> LSig GhcRn
extractPatternSyn nm t tvs cons =
  case filter matches cons of
    [] -> error "extractPatternSyn: constructor pattern not found"
    con:_ -> extract <$> con
 where
  matches :: LConDecl GhcRn -> Bool
  matches (L _ con) = nm `elem` (unLoc <$> getConNames con)
  extract :: ConDecl GhcRn -> Sig GhcRn
  extract con =
    let args =
          case getConArgs con of
            PrefixCon args' -> args'
            RecCon (L _ fields) -> cd_fld_type . unLoc <$> fields
            InfixCon arg1 arg2 -> [arg1, arg2]
        typ = longArrow args (data_ty con)
        typ' =
          case con of
            ConDeclH98 { con_mb_cxt = Just cxt } -> noLoc (HsQualTy noExt cxt typ)
            _ -> typ
        typ'' = noLoc (HsQualTy noExt (noLoc []) typ')
    in PatSynSig noExt [noLoc nm] (mkEmptyImplicitBndrs typ'')

  longArrow :: [LHsType GhcRn] -> LHsType GhcRn -> LHsType GhcRn
  longArrow inputs output = foldr (\x y -> noLoc (HsFunTy noExt x y)) output inputs

  data_ty con
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise = foldl' (\x y -> noLoc (HsAppTy noExt x y)) (noLoc (HsTyVar noExt NotPromoted (noLoc t))) tvs

extractRecSel :: Name -> Name -> [LHsType GhcRn] -> [LConDecl GhcRn]
              -> LSig GhcRn
extractRecSel _ _ _ [] = error "extractRecSel: selector not found"

extractRecSel nm t tvs (L _ con : rest) =
  case getConArgs con of
    RecCon (L _ fields) | ((l,L _ (ConDeclField _ _nn ty _)) : _) <- matching_fields fields ->
      L l (TypeSig noExt [noLoc nm] (mkEmptySigWcType (noLoc (HsFunTy noExt data_ty (getBangType ty)))))
    _ -> extractRecSel nm t tvs rest
 where
  matching_fields :: [LConDeclField GhcRn] -> [(SrcSpan, LConDeclField GhcRn)]
  matching_fields flds = [ (l,f) | f@(L _ (ConDeclField _ ns _ _)) <- flds
                                 , L l n <- ns, extFieldOcc n == nm ]
  data_ty
    -- ResTyGADT _ ty <- con_res con = ty
    | ConDeclGADT{} <- con = con_res_ty con
    | otherwise = foldl' (\x y -> noLoc (HsAppTy noExt x y)) (noLoc (HsTyVar noExt NotPromoted (noLoc t))) tvs

-- | Keep export items with docs.
pruneExportItems :: [ExportItem GhcRn] -> [ExportItem GhcRn]
pruneExportItems = filter hasDoc
  where
    hasDoc (ExportDecl{expItemMbDoc = (Documentation d _, _)}) = isJust d
    hasDoc _ = True

mkVisibleNames :: Maps -> [ExportItem GhcRn] -> [DocOption] -> [Name]
mkVisibleNames (_, _, _, instMap) exports opts
  | OptHide `elem` opts = []
  | otherwise = let ns = concatMap exportName exports
                in seqList ns `seq` ns
  where
    exportName e@ExportDecl {} = name ++ subs ++ patsyns
      where subs    = map fst (expItemSubDocs e)
            patsyns = concatMap (getMainDeclBinder . fst) (expItemPats e)
            name = case unLoc $ expItemDecl e of
              InstD _ d -> maybeToList $ M.lookup (getInstLoc d) instMap
              decl      -> getMainDeclBinder decl
    exportName ExportNoDecl {} = [] -- we don't count these as visible, since
                                    -- we don't want links to go to them.
    exportName _ = []

seqList :: [a] -> ()
seqList [] = ()
seqList (x : xs) = x `seq` seqList xs
