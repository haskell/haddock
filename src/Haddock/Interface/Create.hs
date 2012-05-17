{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.Create
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Interface.Create (createInterface) where


import Haddock.Types
import Haddock.Options
import Haddock.GhcUtils
import Haddock.Utils
import Haddock.Convert
import Haddock.Interface.LexParseRn

import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Control.Applicative
import Control.Monad
import qualified Data.Traversable as T

import qualified Packages
import qualified Module
import qualified SrcLoc
import GHC hiding (flags)
import HscTypes
import Name
import Bag
import RdrName
import TcRnTypes
import FastString (unpackFS)


-- | Use a 'TypecheckedModule' to produce an 'Interface'.
-- To do this, we need access to already processed modules in the topological
-- sort. That's what's in the 'IfaceMap'.
createInterface :: TypecheckedModule -> [Flag] -> IfaceMap -> InstIfaceMap -> ErrMsgGhc Interface
createInterface tm flags modMap instIfaceMap = do

  let ms            = pm_mod_summary . tm_parsed_module $ tm
      mi            = moduleInfo tm
      safety        = modInfoSafe mi
      mdl           = ms_mod ms
      dflags        = ms_hspp_opts ms
      instances     = modInfoInstances mi
      exportedNames = modInfoExports mi

      (TcGblEnv {tcg_rdr_env = gre, tcg_warns = warnings}, _) = tm_internals_ tm

  -- The renamed source should always be available to us, but it's best
  -- to be on the safe side.
  (group_, mayExports, mayDocHeader) <-
    case renamedSource tm of
      Nothing -> do
        liftErrMsg $ tell [ "Warning: Renamed source is not available." ]
        return (emptyRnGroup, Nothing, Nothing)
      Just (x, _, y, z) -> return (x, y, z)

  opts0 <- liftErrMsg $ mkDocOpts (haddockOptions dflags) flags mdl
  let opts
        | Flag_IgnoreAllExports `elem` flags = OptIgnoreExports : opts0
        | otherwise = opts0

  (info, mbDoc) <- liftErrMsg $ processModuleHeader dflags gre safety mayDocHeader

  let declsWithDocs = topDecls group_
      (decls, _) = unzip declsWithDocs
      localInsts = filter (nameIsLocalOrFrom mdl . getName) instances

  maps@(docMap, argMap, subMap, declMap) <-
    liftErrMsg $ mkMaps dflags gre localInsts declsWithDocs

  let exports0 = fmap (reverse . map unLoc) mayExports
      exports
       | OptIgnoreExports `elem` opts = Nothing
       | otherwise = exports0

  liftErrMsg $ warnAboutFilteredDecls mdl decls

  exportItems <- mkExportItems modMap mdl warnings gre exportedNames decls maps exports
                   instances instIfaceMap dflags

  let visibleNames = mkVisibleNames exportItems opts

  -- Measure haddock documentation coverage.
  let prunedExportItems0 = pruneExportItems exportItems
      haddockable = 1 + length exportItems -- module + exports
      haddocked = (if isJust mbDoc then 1 else 0) + length prunedExportItems0
      coverage = (haddockable, haddocked)

  -- Prune the export list to just those declarations that have
  -- documentation, if the 'prune' option is on.
  let prunedExportItems
        | OptPrune `elem` opts = prunedExportItems0
        | otherwise = exportItems

  let aliases =
        mkAliasMap dflags $ tm_renamed_source tm

  return Interface {
    ifaceMod             = mdl,
    ifaceOrigFilename    = msHsFilePath ms,
    ifaceInfo            = info,
    ifaceDoc             = Documentation mbDoc (moduleWarning warnings),
    ifaceRnDoc           = Documentation Nothing Nothing,
    ifaceOptions         = opts,
    ifaceDocMap          = docMap,
    ifaceArgMap          = argMap,
    ifaceRnDocMap        = M.empty,
    ifaceRnArgMap        = M.empty,
    ifaceExportItems     = prunedExportItems,
    ifaceRnExportItems   = [],
    ifaceExports         = exportedNames,
    ifaceVisibleExports  = visibleNames,
    ifaceDeclMap         = declMap,
    ifaceSubMap          = subMap,
    ifaceModuleAliases   = aliases,
    ifaceInstances       = instances,
    ifaceHaddockCoverage = coverage
  }

mkAliasMap :: DynFlags -> Maybe RenamedSource -> M.Map Module ModuleName
mkAliasMap dflags mRenamedSource =
  case mRenamedSource of
    Nothing -> M.empty
    Just (_,impDecls,_,_) ->
      M.fromList $
      mapMaybe (\(SrcLoc.L _ impDecl) -> do
        alias <- ideclAs impDecl
        return $
          (lookupModuleDyn dflags
             (fmap Module.fsToPackageId $
              ideclPkgQual impDecl)
             (case ideclName impDecl of SrcLoc.L _ name -> name),
           alias))
        impDecls

-- similar to GHC.lookupModule
lookupModuleDyn ::
  DynFlags -> Maybe PackageId -> ModuleName -> Module
lookupModuleDyn _ (Just pkgId) mdlName =
  Module.mkModule pkgId mdlName
lookupModuleDyn dflags Nothing mdlName =
  flip Module.mkModule mdlName $
  case filter snd $
       Packages.lookupModuleInAllPackages dflags mdlName of
    (pkgId,_):_ -> Packages.packageConfigId pkgId
    [] -> Module.mainPackageId


-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------


lookupWarning :: Warnings -> GlobalRdrEnv -> Name -> Maybe (Doc id)
lookupWarning NoWarnings  _ _ = Nothing
lookupWarning (WarnAll _) _ _ = Nothing
lookupWarning (WarnSome ws) gre name =
  -- there is at most one warning for each name, so it's fine to use
  -- listToMaybe here
  listToMaybe [warnToDoc w
              | (occ, w) <- ws
              , elt <- lookupGlobalRdrEnv gre occ
              , gre_name elt == name
              ]


moduleWarning :: Warnings -> Maybe (Doc id)
moduleWarning ws =
  case ws of
    NoWarnings -> Nothing
    WarnSome _ -> Nothing
    WarnAll w  -> Just (warnToDoc w)


warnToDoc :: WarningTxt -> Doc id
warnToDoc w = case w of
  (DeprecatedTxt msg) -> format "Deprecated: " msg
  (WarningTxt    msg) -> format "Warning: "    msg
  where
    format x xs = DocWarning . DocParagraph . DocString . concat $ x : map unpackFS xs


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
  if Flag_HideModule (moduleString mdl) `elem` flags
    then return $ OptHide : opts
    else return opts


parseOption :: String -> ErrMsgM (Maybe DocOption)
parseOption "hide"           = return (Just OptHide)
parseOption "prune"          = return (Just OptPrune)
parseOption "ignore-exports" = return (Just OptIgnoreExports)
parseOption "not-home"       = return (Just OptNotHome)
parseOption other = tell ["Unrecognised option: " ++ other] >> return Nothing


--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------


type Maps = (DocMap Name, ArgMap Name, SubMap, DeclMap)


-- | Create 'Maps' by looping through the declarations. For each declaration,
-- find its names, its subordinates, and its doc strings. Process doc strings
-- into 'Doc's.
mkMaps :: DynFlags
       -> GlobalRdrEnv
       -> [Instance]
       -> [(LHsDecl Name, [HsDocString])]
       -> ErrMsgM Maps
mkMaps dflags gre instances decls = do
  (a, b, c, d) <- unzip4 <$> mapM mappings decls
  return (f a, f b, f c, f d)
  where
    f :: (Ord a, Monoid b) => [[(a, b)]] -> Map a b
    f = M.fromListWith (<>) . concat

    mappings (ldecl, docStrs) = do
      let decl = unLoc ldecl
      let declDoc strs m = do
            doc <- processDocStrings dflags gre strs
            m' <- M.mapMaybe id <$> T.mapM (processDocStringParas dflags gre) m
            return (doc, m')
      (doc, args) <- declDoc docStrs (typeDocs decl)
      let subs = subordinates decl
      (subDocs, subArgs) <- unzip <$> mapM (\(_, strs, m) -> declDoc strs m) subs
      let ns = names decl
          subNs = [ n | (n, _, _) <- subs ]
          dm = [ (n, d) | (n, Just d) <- zip ns (repeat doc) ++ zip subNs subDocs ]
          am = [ (n, args) | n <- ns ] ++ zip subNs subArgs
          sm = [ (n, subNs) | n <- ns ]
          cm = [ (n, [ldecl]) | n <- ns ++ subNs ]
      return (dm, am, sm, cm)

    instanceMap :: Map SrcSpan Name
    instanceMap = M.fromList [ (getSrcSpan n, n) | i <- instances, let n = getName i ]

    names :: HsDecl Name -> [Name]
    names (InstD (InstDecl (L l _) _ _ _)) = maybeToList (M.lookup l instanceMap)  -- See note [2].
    names decl = getMainDeclBinder decl


-- Note [2]:
------------
-- We relate Instances to InstDecls using the SrcSpans buried inside them.
-- That should work for normal user-written instances (from looking at GHC
-- sources). We can assume that commented instances are user-written.
-- This lets us relate Names (from Instances) to comments (associated
-- with InstDecls).


--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------


-- | Get all subordinate declarations inside a declaration, and their docs.
subordinates :: HsDecl Name -> [(Name, [HsDocString], Map Int HsDocString)]
subordinates (TyClD decl)
  | isClassDecl decl = classSubs
  | isDataDecl  decl = dataSubs
  where
    classSubs = [ (name, doc, typeDocs d) | (L _ d, doc) <- classDecls decl
                , name <- getMainDeclBinder d, not (isValD d)
                ]
    dataSubs = constrs ++ fields
      where
        cons = map unL $ tcdCons decl
        constrs = [ (unL $ con_name c, maybeToList $ fmap unL $ con_doc c, M.empty)
                  | c <- cons ]
        fields  = [ (unL n, maybeToList $ fmap unL doc, M.empty)
                  | RecCon flds <- map con_details cons
                  , ConDeclField n _ doc <- flds ]
subordinates _ = []


-- | Extract function argument docs from inside types.
typeDocs :: HsDecl Name -> Map Int HsDocString
typeDocs d =
  let docs = go 0 in
  case d of
    SigD (TypeSig _ ty) -> docs (unLoc ty)
    ForD (ForeignImport _ ty _ _) -> docs (unLoc ty)
    TyClD (TySynonym {tcdSynRhs = ty}) -> docs (unLoc ty)
    _ -> M.empty
  where
    go n (HsForAllTy _ _ _ ty) = go n (unLoc ty)
    go n (HsFunTy (L _ (HsDocTy _ (L _ x))) (L _ ty)) = M.insert n x $ go (n+1) ty
    go n (HsFunTy _ ty) = go (n+1) (unLoc ty)
    go n (HsDocTy _ (L _ doc)) = M.singleton n doc
    go _ _ = M.empty


-- | All the sub declarations of a class (that we handle), ordered by
-- source location, with documentation attached if it exists.
classDecls :: TyClDecl Name -> [(LHsDecl Name, [HsDocString])]
classDecls class_ = filterDecls . collectDocs . sortByLoc $ decls
  where
    decls = docs ++ defs ++ sigs ++ ats
    docs  = mkDecls tcdDocs DocD class_
    defs  = mkDecls (bagToList . tcdMeths) ValD class_
    sigs  = mkDecls tcdSigs SigD class_
    ats   = mkDecls tcdATs TyClD class_


-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup Name -> [(LHsDecl Name, [HsDocString])]
topDecls = filterClasses . filterDecls . collectDocs . sortByLoc . ungroup


-- | Take all declarations except pragmas, infix decls, rules from an 'HsGroup'.
ungroup :: HsGroup Name -> [LHsDecl Name]
ungroup group_ =
  mkDecls (concat   . hs_tyclds) TyClD  group_ ++
  mkDecls hs_derivds             DerivD group_ ++
  mkDecls hs_defds               DefD   group_ ++
  mkDecls hs_fords               ForD   group_ ++
  mkDecls hs_docs                DocD   group_ ++
  mkDecls hs_instds              InstD  group_ ++
  mkDecls (typesigs . hs_valds)  SigD   group_ ++
  mkDecls (valbinds . hs_valds)  ValD   group_
  where
    typesigs (ValBindsOut _ sigs) = filter isVanillaLSig sigs
    typesigs _ = error "expected ValBindsOut"

    valbinds (ValBindsOut binds _) = concatMap bagToList . snd . unzip $ binds
    valbinds _ = error "expected ValBindsOut"


-- | Take a field of declarations from a data structure and create HsDecls
-- using the given constructor
mkDecls :: (a -> [Located b]) -> (b -> c) -> a -> [Located c]
mkDecls field con struct = [ L loc (con decl) | L loc decl <- field struct ]


-- | Sort by source location
sortByLoc :: [Located a] -> [Located a]
sortByLoc = sortBy (comparing getLoc)


warnAboutFilteredDecls :: Module -> [LHsDecl Name] -> ErrMsgM ()
warnAboutFilteredDecls mdl decls = do
  let modStr = moduleString mdl
  let typeInstances =
        nub [ tcdName d | L _ (TyClD d) <- decls, isFamInstDecl d ]

  unless (null typeInstances) $
    tell [
      "Warning: " ++ modStr ++ ": Instances of type and data "
      ++ "families are not yet supported. Instances of the following families "
      ++ "will be filtered out:\n  " ++ (intercalate ", "
      $ map (occNameString . nameOccName) typeInstances) ]

  let instances = nub [ pretty i | L _ (InstD (InstDecl i _ _ ats)) <- decls
                                 , not (null ats) ]

  unless (null instances) $
    tell [
      "Warning: " ++ modStr ++ ": We do not support associated types in instances yet. "
      ++ "These instances are affected:\n" ++ intercalate ", " instances ]


--------------------------------------------------------------------------------
-- Filtering of declarations
--
-- We filter out declarations that we don't intend to handle later.
--------------------------------------------------------------------------------


-- | Filter out declarations that we don't handle in Haddock
filterDecls :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterDecls = filter (isHandled . unL . fst)
  where
    isHandled (ForD (ForeignImport {})) = True
    isHandled (TyClD {}) = True
    isHandled (InstD {}) = True
    isHandled (SigD d) = isVanillaLSig (reL d)
    isHandled (ValD _) = True
    -- we keep doc declarations to be able to get at named docs
    isHandled (DocD _) = True
    isHandled _ = False


-- | Go through all class declarations and filter their sub-declarations
filterClasses :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterClasses decls = [ if isClassD d then (L loc (filterClass d), doc) else x
                      | x@(L loc d, doc) <- decls ]
  where
    filterClass (TyClD c) =
      TyClD $ c { tcdSigs = filter isVanillaLSig $ tcdSigs c }
    filterClass _ = error "expected TyClD"


--------------------------------------------------------------------------------
-- Collect docs
--
-- To be able to attach the right Haddock comment to the right declaration,
-- we sort the declarations by their SrcLoc and "collect" the docs for each
-- declaration.
--------------------------------------------------------------------------------


-- | Collect docs and attach them to the right declarations.
collectDocs :: [LHsDecl a] -> [(LHsDecl a, [HsDocString])]
collectDocs = go Nothing []
  where
    go Nothing _ [] = []
    go (Just prev) docs [] = finished prev docs []
    go prev docs (L _ (DocD (DocCommentNext str)) : ds)
      | Nothing <- prev = go Nothing (str:docs) ds
      | Just decl <- prev = finished decl docs (go Nothing [str] ds)
    go prev docs (L _ (DocD (DocCommentPrev str)) : ds) = go prev (str:docs) ds
    go Nothing docs (d:ds) = go (Just d) docs ds
    go (Just prev) docs (d:ds) = finished prev docs (go (Just d) [] ds)

    finished decl docs rest = (decl, reverse docs) : rest


-- | Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.
--
-- We create the export items even if the module is hidden, since they
-- might be useful when creating the export items for other modules.
mkExportItems
  :: IfaceMap
  -> Module             -- this module
  -> Warnings
  -> GlobalRdrEnv
  -> [Name]             -- exported names (orig)
  -> [LHsDecl Name]
  -> Maps
  -> Maybe [IE Name]
  -> [Instance]
  -> InstIfaceMap
  -> DynFlags
  -> ErrMsgGhc [ExportItem Name]
mkExportItems
  modMap thisMod warnings gre exportedNames decls0
  (maps@(docMap, argMap, subMap, declMap)) optExports _ instIfaceMap dflags =
  case optExports of
    Nothing -> fullModuleContents dflags warnings gre maps decls
    Just exports -> liftM concat $ mapM lookupExport exports
  where
    decls = filter (not . isInstD . unLoc) decls0


    lookupExport (IEVar x)             = declWith x
    lookupExport (IEThingAbs t)        = declWith t
    lookupExport (IEThingAll t)        = declWith t
    lookupExport (IEThingWith t _)     = declWith t
    lookupExport (IEModuleContents m)  =
      moduleExports thisMod m dflags warnings gre exportedNames decls modMap instIfaceMap maps
    lookupExport (IEGroup lev docStr)  = liftErrMsg $
      ifDoc (processDocString dflags gre docStr)
            (\doc -> return [ ExportGroup lev "" doc ])
    lookupExport (IEDoc docStr)        = liftErrMsg $
      ifDoc (processDocStringParas dflags gre docStr)
            (\doc -> return [ ExportDoc doc ])
    lookupExport (IEDocNamed str)      = liftErrMsg $
      ifDoc (findNamedDoc str [ unL d | d <- decls ])
            (\docStr ->
            ifDoc (processDocStringParas dflags gre docStr)
                  (\doc -> return [ ExportDoc doc ]))


    ifDoc :: (Monad m) => m (Maybe a) -> (a -> m [b]) -> m [b]
    ifDoc parse finish = do
      mbDoc <- parse
      case mbDoc of Nothing -> return []; Just doc -> finish doc


    declWith :: Name -> ErrMsgGhc [ ExportItem Name ]
    declWith t =
      case findDecl t of
        ([L _ (ValD _)], (doc, _)) -> do
          -- Top-level binding without type signature
          export <- hiValExportItem t doc
          return [export]
        (ds, docs_) | decl : _ <- filter (not . isValD . unLoc) ds ->
          let declNames = getMainDeclBinder (unL decl)
          in case () of
            _
              -- temp hack: we filter out separately exported ATs, since we haven't decided how
              -- to handle them yet. We should really give an warning message also, and filter the
              -- name out in mkVisibleNames...
              | t `elem` declATs (unL decl)        -> return []

              -- We should not show a subordinate by itself if any of its
              -- parents is also exported. See note [1].
              | t `notElem` declNames,
                Just p <- find isExported (parents t $ unL decl) ->
                do liftErrMsg $ tell [
                     "Warning: " ++ moduleString thisMod ++ ": " ++
                     pretty (nameOccName t) ++ " is exported separately but " ++
                     "will be documented under " ++ pretty (nameOccName p) ++
                     ". Consider exporting it together with its parent(s)" ++
                     " for code clarity." ]
                   return []

              -- normal case
              | otherwise -> return [ mkExportDecl t newDecl docs_ ]
                  where
                    -- A single signature might refer to many names, but we
                    -- create an export item for a single name only.  So we
                    -- modify the signature to contain only that single name.
                    newDecl = case decl of
                      (L loc (SigD sig)) ->
                        L loc . SigD . fromJust $ filterSigNames (== t) sig
                        -- fromJust is safe since we already checked in guards
                        -- that 't' is a name declared in this declaration.
                      _                  -> decl

        -- Declaration from another package
        ([], _) -> do
          mayDecl <- hiDecl t
          case mayDecl of
            Nothing -> return [ ExportNoDecl t [] ]
            Just decl ->
              -- We try to get the subs and docs
              -- from the installed .haddock file for that package.
              case M.lookup (nameModule t) instIfaceMap of
                Nothing -> do
                   liftErrMsg $ tell
                      ["Warning: Couldn't find .haddock for export " ++ pretty t]
                   let subs_ = [ (n, noDocForDecl) | (n, _, _) <- subordinates (unLoc decl) ]
                   return [ mkExportDecl t decl (noDocForDecl, subs_) ]
                Just iface ->
                   return [ mkExportDecl t decl (lookupDocs t warnings gre (instDocMap iface) (instArgMap iface) (instSubMap iface)) ]

        _ -> return []


    mkExportDecl :: Name -> LHsDecl Name -> (DocForDecl Name, [(Name, DocForDecl Name)]) -> ExportItem Name
    mkExportDecl n decl (doc, subs) = decl'
      where
        decl' = ExportDecl (restrictTo sub_names (extractDecl n mdl decl)) doc subs' []
        mdl = nameModule n
        subs' = filter (isExported . fst) subs
        sub_names = map fst subs'


    isExported = (`elem` exportedNames)


    findDecl :: Name -> ([LHsDecl Name], (DocForDecl Name, [(Name, DocForDecl Name)]))
    findDecl n
      | m == thisMod, Just ds <- M.lookup n declMap =
          (ds, lookupDocs n warnings gre docMap argMap subMap)
      | Just iface <- M.lookup m modMap, Just ds <- M.lookup n (ifaceDeclMap iface) =
          (ds, lookupDocs n warnings gre (ifaceDocMap iface) (ifaceArgMap iface) (ifaceSubMap iface))
      | otherwise = ([], (noDocForDecl, []))
      where
        m = nameModule n


hiDecl :: Name -> ErrMsgGhc (Maybe (LHsDecl Name))
hiDecl t = do
  mayTyThing <- liftGhcToErrMsgGhc $ lookupName t
  case mayTyThing of
    Nothing -> do
      liftErrMsg $ tell ["Warning: Not found in environment: " ++ pretty t]
      return Nothing
    Just x -> return (Just (tyThingToLHsDecl x))


hiValExportItem :: Name -> DocForDecl Name -> ErrMsgGhc (ExportItem Name)
hiValExportItem name doc = do
  mayDecl <- hiDecl name
  case mayDecl of
    Nothing -> return (ExportNoDecl name [])
    Just decl -> return (ExportDecl decl doc [] [])


-- | Lookup docs for a declaration from maps.
lookupDocs :: Name -> Warnings -> GlobalRdrEnv -> DocMap Name -> ArgMap Name -> SubMap -> (DocForDecl Name, [(Name, DocForDecl Name)])
lookupDocs n warnings gre docMap argMap subMap =
  let lookupArgDoc x = M.findWithDefault M.empty x argMap in
  let doc = (lookupDoc n, lookupArgDoc n) in
  let subs = M.findWithDefault [] n subMap in
  let subDocs = [ (s, (lookupDoc s, lookupArgDoc s)) | s <- subs ] in
  (doc, subDocs)
  where
    lookupDoc name = Documentation (M.lookup name docMap) (lookupWarning warnings gre name)


-- | Return all export items produced by an exported module. That is, we're
-- interested in the exports produced by \"module B\" in such a scenario:
--
-- > module A (module B) where
-- > import B (...) hiding (...)
--
-- There are three different cases to consider:
--
-- 1) B is hidden, in which case we return all its exports that are in scope in A.
-- 2) B is visible, but not all its exports are in scope in A, in which case we
--    only return those that are.
-- 3) B is visible and all its exports are in scope, in which case we return
--    a single 'ExportModule' item. 
moduleExports :: Module           -- ^ Module A
              -> ModuleName       -- ^ The real name of B, the exported module
              -> DynFlags         -- ^ The flags used when typechecking A
              -> Warnings
              -> GlobalRdrEnv     -- ^ The renaming environment used for A
              -> [Name]           -- ^ All the exports of A
              -> [LHsDecl Name]   -- ^ All the declarations in A
              -> IfaceMap         -- ^ Already created interfaces
              -> InstIfaceMap     -- ^ Interfaces in other packages
              -> Maps
              -> ErrMsgGhc [ExportItem Name] -- ^ Resulting export items
moduleExports thisMod expMod dflags warnings gre _exports decls ifaceMap instIfaceMap maps
  | m == thisMod = fullModuleContents dflags warnings gre maps decls
  | otherwise =
    case M.lookup m ifaceMap of
      Just iface
        | OptHide `elem` ifaceOptions iface -> return (ifaceExportItems iface)
        | otherwise -> return [ ExportModule m ]

      Nothing -> -- We have to try to find it in the installed interfaces
                 -- (external packages).
        case M.lookup expMod (M.mapKeys moduleName instIfaceMap) of
          Just iface -> return [ ExportModule (instMod iface) ]
          Nothing -> do
            liftErrMsg $
              tell ["Warning: " ++ pretty thisMod ++ ": Could not find " ++
                    "documentation for exported module: " ++ pretty expMod]
            return []
  where
    m = mkModule packageId expMod
    packageId = modulePackageId thisMod


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


fullModuleContents :: DynFlags -> Warnings -> GlobalRdrEnv -> Maps -> [LHsDecl Name] -> ErrMsgGhc [ExportItem Name]
fullModuleContents dflags warnings gre (docMap, argMap, subMap, declMap) decls =
  liftM catMaybes $ mapM mkExportItem (expandSig decls)
  where
    -- A type signature can have multiple names, like:
    --   foo, bar :: Types..
    --
    -- We go through the list of declarations and expand type signatures, so
    -- that every type signature has exactly one name!
    expandSig :: [LHsDecl name] -> [LHsDecl name]
    expandSig = foldr f []
      where
        f :: LHsDecl name -> [LHsDecl name] -> [LHsDecl name]
        f (L l (SigD (TypeSig    names t))) xs = foldr (\n acc -> L l (SigD (TypeSig    [n] t)) : acc) xs names
        f (L l (SigD (GenericSig names t))) xs = foldr (\n acc -> L l (SigD (GenericSig [n] t)) : acc) xs names
        f x xs = x : xs

    mkExportItem (L _ (DocD (DocGroup lev docStr))) = do
      mbDoc <- liftErrMsg $ processDocString dflags gre docStr
      return $ fmap (ExportGroup lev "") mbDoc
    mkExportItem (L _ (DocD (DocCommentNamed _ docStr))) = do
      mbDoc <- liftErrMsg $ processDocStringParas dflags gre docStr
      return $ fmap ExportDoc mbDoc
    mkExportItem (L _ (ValD d))
      | name:_ <- collectHsBindBinders d, Just [L _ (ValD _)] <- M.lookup name declMap =
          -- Top-level binding without type signature.
          let (doc, _) = lookupDocs name warnings gre docMap argMap subMap in
          fmap Just (hiValExportItem name doc)
      | otherwise = return Nothing
    mkExportItem decl
      | name:_ <- getMainDeclBinder (unLoc decl) =
        let (doc, subs) = lookupDocs name warnings gre docMap argMap subMap in
        return $ Just (ExportDecl decl doc subs [])
      | otherwise = return Nothing


-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble
-- together a type signature for it...).
extractDecl :: Name -> Module -> LHsDecl Name -> LHsDecl Name
extractDecl name mdl decl
  | name `elem` getMainDeclBinder (unLoc decl) = decl
  | otherwise  =
    case unLoc decl of
      TyClD d | isClassDecl d ->
        let matches = [ sig | sig <- tcdSigs d, name `elem` sigName sig,
                        isVanillaLSig sig ] -- TODO: document fixity
        in case matches of
          [s0] -> let (n, tyvar_names) = name_and_tyvars d
                      L pos sig = extractClassDecl n tyvar_names s0
                  in L pos (SigD sig)
          _ -> error "internal: extractDecl"
      TyClD d | isDataDecl d ->
        let (n, tyvar_names) = name_and_tyvars d
            L pos sig = extractRecSel name mdl n tyvar_names (tcdCons d)
        in L pos (SigD sig)
      _ -> error "internal: extractDecl"
  where
    name_and_tyvars d = (unLoc (tcdLName d), hsLTyVarLocNames (tcdTyVars d))


toTypeNoLoc :: Located Name -> LHsType Name
toTypeNoLoc = noLoc . HsTyVar . unLoc


extractClassDecl :: Name -> [Located Name] -> LSig Name -> LSig Name
extractClassDecl c tvs0 (L pos (TypeSig lname ltype)) = case ltype of
  L _ (HsForAllTy expl tvs (L _ preds) ty) ->
    L pos (TypeSig lname (noLoc (HsForAllTy expl tvs (lctxt preds) ty)))
  _ -> L pos (TypeSig lname (noLoc (mkImplicitHsForAllTy (lctxt []) ltype)))
  where
    lctxt = noLoc . ctxt
    ctxt preds = nlHsTyConApp c (map toTypeNoLoc tvs0) : preds
extractClassDecl _ _ _ = error "extractClassDecl: unexpected decl"


extractRecSel :: Name -> Module -> Name -> [Located Name] -> [LConDecl Name]
              -> LSig Name
extractRecSel _ _ _ _ [] = error "extractRecSel: selector not found"

extractRecSel nm mdl t tvs (L _ con : rest) =
  case con_details con of
    RecCon fields | (ConDeclField n ty _ : _) <- matching_fields fields ->
      L (getLoc n) (TypeSig [noLoc nm] (noLoc (HsFunTy data_ty (getBangType ty))))
    _ -> extractRecSel nm mdl t tvs rest
 where
  matching_fields flds = [ f | f@(ConDeclField n _ _) <- flds, unLoc n == nm ]
  data_ty = foldl (\x y -> noLoc (HsAppTy x y)) (noLoc (HsTyVar t)) (map toTypeNoLoc tvs)


-- | Keep exprt items with docs.
pruneExportItems :: [ExportItem Name] -> [ExportItem Name]
pruneExportItems = filter hasDoc
  where
    hasDoc (ExportDecl{expItemMbDoc = (Documentation d _, _)}) = isJust d
    hasDoc _ = True


mkVisibleNames :: [ExportItem Name] -> [DocOption] -> [Name]
mkVisibleNames exports opts
  | OptHide `elem` opts = []
  | otherwise = concatMap exportName exports
  where
    exportName e@ExportDecl {} = getMainDeclBinder (unL $ expItemDecl e) ++ subs
      where subs = map fst (expItemSubDocs e)
    exportName ExportNoDecl {} = [] -- we don't count these as visible, since
                                    -- we don't want links to go to them.
    exportName _ = []


-- | Find a stand-alone documentation comment by its name.
findNamedDoc :: String -> [HsDecl Name] -> ErrMsgM (Maybe HsDocString)
findNamedDoc name = search
  where
    search [] = do
      tell ["Cannot find documentation for: $" ++ name]
      return Nothing
    search (DocD (DocCommentNamed name' doc) : rest)
      | name == name' = return (Just doc)
      | otherwise = search rest
    search (_other_decl : rest) = search rest
