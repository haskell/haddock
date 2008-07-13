--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Interface.Create (createInterface) where


import Haddock.Types
import Haddock.Options
import Haddock.GHC.Utils
import Haddock.Utils

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad

import GHC
import Outputable
import SrcLoc
import Name
import Module
import InstEnv
import Class
import TypeRep
import Var hiding (varName)
import TyCon
import PrelNames
import Bag
import HscTypes


-- | Process the data in the GhcModule to produce an interface.
-- To do this, we need access to already processed modules in the topological
-- sort. That's what's in the module map.
createInterface :: GhcModule -> [Flag] -> ModuleMap -> ErrMsgM Interface
createInterface ghcMod flags modMap = do

  let mod = ghcModule ghcMod

  opts0 <- mkDocOpts (ghcMbDocOpts ghcMod) flags mod
  let opts
        | Flag_IgnoreAllExports `elem` flags = OptIgnoreExports : opts0
        | otherwise = opts0

  let group         = ghcGroup ghcMod
      exports       = fmap (reverse . map unLoc) (ghcMbExports ghcMod)
      localNames    = ghcDefinedNames ghcMod
      subMap        = mkSubMap group
      decls         = topDecls group
      declMap       = mkDeclMap decls
      ignoreExps    = Flag_IgnoreAllExports `elem` flags
      exportedNames = ghcExportedNames ghcMod
      origEnv       = Map.fromList [ (nameOccName n, n) | n <- exportedNames ]

  visibleNames <- mkVisibleNames mod modMap localNames 
                                 (ghcNamesInScope ghcMod) 
                                 subMap exports opts declMap 

  exportItems <- mkExportItems modMap mod (ghcExportedNames ghcMod)
                               decls declMap subMap opts exports ignoreExps

  -- prune the export list to just those declarations that have
  -- documentation, if the 'prune' option is on.
  let 
    prunedExportItems
      | OptPrune `elem` opts = pruneExportItems exportItems
      | otherwise = exportItems
 
  return Interface {
    ifaceMod             = mod,
    ifaceOrigFilename    = ghcFilename ghcMod,
    ifaceInfo            = ghcHaddockModInfo ghcMod,
    ifaceDoc             = ghcMbDoc ghcMod,
    ifaceRnDoc           = Nothing,
    ifaceOptions         = opts,
    ifaceLocals          = localNames,
    ifaceRnDocMap        = Map.empty,
    ifaceSubMap          = subMap,
    ifaceExportItems     = prunedExportItems,
    ifaceRnExportItems   = [],
    ifaceEnv             = origEnv, 
    ifaceExports         = exportedNames,
    ifaceVisibleExports  = visibleNames, 
    ifaceDeclMap         = declMap,
    ifaceInstances       = ghcInstances ghcMod
  }


-------------------------------------------------------------------------------
-- Doc options
--
-- Haddock options that are embedded in the source file
-------------------------------------------------------------------------------


mkDocOpts :: Maybe String -> [Flag] -> Module -> ErrMsgM [DocOption]
mkDocOpts mbOpts flags mod = do
  opts <- case mbOpts of 
    Just opts -> case words $ replace ',' ' ' opts of
      [] -> tell ["No option supplied to DOC_OPTION/doc_option"] >> return []
      xs -> liftM catMaybes (mapM parseOption xs)
    Nothing -> return []
  if Flag_HideModule (moduleString mod) `elem` flags 
    then return $ OptHide : opts
    else return opts


parseOption :: String -> ErrMsgM (Maybe DocOption)
parseOption "hide"           = return (Just OptHide)
parseOption "prune"          = return (Just OptPrune)
parseOption "ignore-exports" = return (Just OptIgnoreExports)
parseOption "not-home"       = return (Just OptNotHome)
parseOption other = tell ["Unrecognised option: " ++ other] >> return Nothing


--------------------------------------------------------------------------------
-- Extract declarations
--------------------------------------------------------------------------------


-- | Make a map from names to declarations with documentation. The map excludes
-- all kinds of instance declarations (including type family instances) and
-- documentation declarations.
-- Subordinate names are mapped to the parent declaration, but with the doc
-- for the subordinate declaration.
mkDeclMap decls = Map.fromList [ (n, (L loc d, doc)) | (L loc d, doc) <- decls 
                               , (n, doc) <- (declName d, doc) : subordinates d
                               , notDocOrInstance d ]


notDocOrInstance (InstD _) = False
notDocOrInstance (TyClD (d@TyData {}))
  | Just _ <- tcdTyPats d = False
notDocOrInstance (TyClD (d@TySynonym {}))
  | Just _ <- tcdTyPats d = False
notDocOrInstance (DocD _) = False
notDocOrInstance _        = True


subordinates (TyClD d) = classDataSubs d
subordinates _ = []


classDataSubs :: TyClDecl Name -> [(Name, Maybe (HsDoc Name))]
classDataSubs decl
  | isClassDecl decl = classMeths
  | isDataDecl  decl = recordFields
  | otherwise        = []
  where
    classMeths   = [ (declName d, doc) | (L _ d, doc) <- classDecls decl ]
    recordFields = [ (unLoc lname, fmap unLoc doc) |
                     ConDeclField lname _ doc <- fields ]
    cons         = [ con | L _ con <- tcdCons decl ]
    fields       = concat [ fields | RecCon fields <- map con_details cons]


-- All the sub declarations of a class (except default methods), ordered by
-- source location, with documentation attached if it exists. 
classDecls = filter notDef . collectDocs . sortByLoc . declsFromClass
  where
    notDef (L _ (ValD _), _) = False
    notDef _                 = True  


declsFromClass class_ = docs ++ defs ++ sigs ++ ats
  where 
    docs = decls tcdDocs DocD class_
    defs = decls (bagToList . tcdMeths) ValD class_
    sigs = decls tcdSigs SigD class_
    ats  = decls tcdATs TyClD class_


declName (TyClD d) = tcdName d
declName (ForD (ForeignImport n _ _)) = unLoc n
-- we have normal sigs only (since they are taken from ValBindsOut)
declName (SigD sig) = fromJust $ sigNameNoLoc sig


-- All the top-level declarations of a module, ordered by source location,
-- with documentation attached if it exists
topDecls :: HsGroup Name -> [(LHsDecl Name, Maybe (HsDoc Name))] 
topDecls = collectDocs . sortByLoc . declsFromGroup


-- | Pick out the declarations that we want from a group
declsFromGroup :: HsGroup Name -> [LHsDecl Name] 
declsFromGroup group = 
  decls hs_tyclds TyClD group ++
  decls hs_fords  ForD  group ++
  decls hs_docs   DocD  group ++
  decls (sigs . hs_valds) SigD group
  where
    sigs (ValBindsOut _ x) = x


-- | Takes a field of declarations from a data structure and creates HsDecls
-- using the given constructor
decls field con struct = [ L loc (con decl) | L loc decl <- field struct ]


-- | Sort by source location
sortByLoc = sortBy (comparing getLoc)


--------------------------------------------------------------------------------
-- Collect docs
--
-- To be able to attach the right Haddock comment to the right declaration,
-- we sort the declarations by their SrcLoc and "collect" the docs for each 
-- declaration.
--------------------------------------------------------------------------------


-- | Collect the docs and attach them to the right declaration
collectDocs :: [LHsDecl Name] -> [(LHsDecl Name, Maybe (HsDoc Name))]
collectDocs decls = collect Nothing DocEmpty decls


collect :: Maybe (LHsDecl Name) -> HsDoc Name -> [LHsDecl Name] -> [(LHsDecl Name, Maybe (HsDoc Name))]
collect d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (e:es) =
  case e of
    L _ (DocD (DocCommentNext str)) ->
      case d of
        Nothing -> collect d (docAppend doc_so_far str) es
        Just d0 -> finishedDoc d0 doc_so_far (collect Nothing str es)

    L _ (DocD (DocCommentPrev str)) -> collect d (docAppend doc_so_far str) es

    _ -> case d of
      Nothing -> collect (Just e) doc_so_far es
      Just d0
        | sameDecl d0 e -> collect d doc_so_far es  
        | otherwise -> finishedDoc d0 doc_so_far (collect (Just e) DocEmpty es)


finishedDoc :: LHsDecl Name -> HsDoc Name -> [(LHsDecl Name, Maybe (HsDoc Name))] -> 
               [(LHsDecl Name, Maybe (HsDoc Name))]
finishedDoc d DocEmpty rest = (d, Nothing) : rest
finishedDoc d doc rest | notDocDecl d = (d, Just doc) : rest
  where
    notDocDecl (L _ (DocD _)) = False
    notDocDecl _              = True
finishedDoc _ _ rest = rest


sameDecl d1 d2 = getLoc d1 == getLoc d2

       
mkSubMap :: HsGroup Name -> Map Name [Name]
mkSubMap group = Map.fromList [ (name, subs) | L _ tycld <- hs_tyclds group,
 let name:subs = map unLoc (tyClDeclNames tycld) ]


-- | Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.
mkExportItems
  :: ModuleMap
  -> Module			-- this module
  -> [Name]			-- exported names (orig)
  -> [(LHsDecl Name, Maybe (HsDoc Name))]
  -> Map Name (LHsDecl Name, Maybe (HsDoc Name)) -- maps local names to declarations
  -> Map Name [Name]	-- sub-map for this module
  -> [DocOption]
  -> Maybe [IE Name]
  -> Bool				-- --ignore-all-exports flag
  -> ErrMsgM [ExportItem Name]

mkExportItems modMap this_mod exported_names decls declMap sub_map
              opts maybe_exps ignore_all_exports
  | isNothing maybe_exps || ignore_all_exports || OptIgnoreExports `elem` opts
    = everything_local_exported
  | Just specs <- maybe_exps = do 
      exps <- mapM lookupExport specs
      return (concat exps)
  where
    everything_local_exported =  -- everything exported
      return (fullContentsOfThisModule this_mod decls)
   
    packageId = modulePackageId this_mod

    lookupExport (IEVar x)             = declWith x
    lookupExport (IEThingAbs t)        = declWith t
    lookupExport (IEThingAll t)        = declWith t
    lookupExport (IEThingWith t cs)    = declWith t
    lookupExport (IEModuleContents m)  = fullContentsOf (mkModule packageId m)
    lookupExport (IEGroup lev doc)     = return [ ExportGroup lev "" doc ]
    lookupExport (IEDoc doc)           = return [ ExportDoc doc ] 
    lookupExport (IEDocNamed str) = do
      r <- findNamedDoc str (map (unLoc . fst) decls)
      case r of
        Nothing -> return []
        Just found -> return [ ExportDoc found ]
 
    declWith :: Name -> ErrMsgM [ ExportItem Name ]
    declWith t
	| Just (decl, maybeDoc) <- findDecl t
        = return [ ExportDecl t (restrictTo subs (extractDecl t mdl decl)) maybeDoc [] ]
	| otherwise
	= return []
	where 
              mdl = nameModule t
	      subs = filter (`elem` exported_names) all_subs
              all_subs | mdl == this_mod = Map.findWithDefault [] t sub_map
		       | otherwise       = allSubsOfName modMap t

    fullContentsOf m  
	| m == this_mod = return (fullContentsOfThisModule this_mod decls)
	| otherwise = 
	   case Map.lookup m modMap of
	     Just iface
		| OptHide `elem` ifaceOptions iface
			-> return (ifaceExportItems iface)
		| otherwise -> return [ ExportModule m ]
	     Nothing -> return [] -- already emitted a warning in visibleNames

    findDecl :: Name -> Maybe (LHsDecl Name, Maybe (HsDoc Name))
    findDecl n 
	    | m == this_mod = Map.lookup n declMap
      | otherwise = case Map.lookup m modMap of
                      Just iface -> Map.lookup n (ifaceDeclMap iface) 
                      Nothing -> Nothing
      where
        m = nameModule n


fullContentsOfThisModule :: Module -> [(LHsDecl Name, Maybe (HsDoc Name))] -> [ExportItem Name]
fullContentsOfThisModule module_ decls = catMaybes (map mkExportItem decls)
  where
    mkExportItem (L _ (DocD (DocGroup lev doc)), _) = Just $ ExportGroup lev "" doc
    mkExportItem (decl, doc) = Just $ ExportDecl (declName (unLoc decl)) decl doc []
--    mkExportItem _ = Nothing -- TODO: see if this is really needed


-- | Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble 
-- together a type signature for it...)
extractDecl :: Name -> Module -> LHsDecl Name -> LHsDecl Name
extractDecl name mdl decl
  | Just n <- getMainDeclBinder (unLoc decl), n == name = decl
  | otherwise  =  
    case unLoc decl of
      TyClD d | isClassDecl d -> 
        let matches = [ sig | sig <- tcdSigs d, sigName sig == Just name,
                        isVanillaLSig sig ] -- TODO: document fixity
        in case matches of 
          [s0] -> let (n, tyvar_names) = name_and_tyvars d
                      L pos sig = extractClassDecl n mdl tyvar_names s0
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
toTypeNoLoc lname = noLoc (HsTyVar (unLoc lname))


rmLoc :: Located a -> Located a
rmLoc a = noLoc (unLoc a)


extractClassDecl :: Name -> Module -> [Located Name] -> LSig Name -> LSig Name
extractClassDecl c mdl tvs0 (L pos (TypeSig lname ltype)) = case ltype of
  L _ (HsForAllTy exp tvs (L _ preds) ty) -> 
    L pos (TypeSig lname (noLoc (HsForAllTy exp tvs (lctxt preds) ty)))
  _ -> L pos (TypeSig lname (noLoc (mkImplicitHsForAllTy (lctxt []) ltype)))
  where
    lctxt preds = noLoc (ctxt preds)
    ctxt preds = [noLoc (HsClassP c (map toTypeNoLoc tvs0))] ++ preds  

extractClassDecl _ _ _ d = error $ "extractClassDecl: unexpected decl"


extractRecSel :: Name -> Module -> Name -> [Located Name] -> [LConDecl Name]
              -> LSig Name
extractRecSel _ _ _ _ [] = error "extractRecSel: selector not found"

extractRecSel nm mdl t tvs (L _ con : rest) =
  case con_details con of
    RecCon fields | (ConDeclField n ty _ : _) <- matching_fields fields -> 
      L (getLoc n) (TypeSig (noLoc nm) (noLoc (HsFunTy data_ty (getBangType ty))))
    _ -> extractRecSel nm mdl t tvs rest
 where 
  matching_fields flds = [ f | f@(ConDeclField n _ _) <- flds, (unLoc n) == nm ]   
  data_ty = foldl (\x y -> noLoc (HsAppTy x y)) (noLoc (HsTyVar t)) (map toTypeNoLoc tvs)


-- Pruning
pruneExportItems :: [ExportItem Name] -> [ExportItem Name]
pruneExportItems items = filter hasDoc items
  where hasDoc (ExportDecl _ _ d _) = isJust d
	hasDoc _ = True


-- | Gather a list of original names exported from this module
mkVisibleNames :: Module 
             -> ModuleMap
             -> [Name] 
             -> [Name]
             -> Map Name [Name]
             -> Maybe [IE Name]
             -> [DocOption]
             -> Map Name (LHsDecl Name, Maybe (HsDoc Name))
             -> ErrMsgM [Name]

mkVisibleNames mdl modMap localNames scope subMap maybeExps opts declMap 
  -- if no export list, just return all local names 
  | Nothing <- maybeExps         = return (filter hasDecl localNames)
  | OptIgnoreExports `elem` opts = return localNames
  | Just expspecs <- maybeExps = do
      visibleNames <- mapM extract expspecs
      return $ filter isNotPackageName (concat visibleNames)
 where
  hasDecl name = isJust (Map.lookup name declMap)
  isNotPackageName name = nameMod == mdl || isJust (Map.lookup nameMod modMap)
    where nameMod = nameModule name

  extract e = 
   case e of
    IEVar x -> return [x]
    IEThingAbs t -> return [t]
    IEThingAll t -> return (t : all_subs)
	 where
	      all_subs | nameModule t == mdl = Map.findWithDefault [] t subMap
		       | otherwise = allSubsOfName modMap t

    IEThingWith t cs -> return (t : cs)
	
    IEModuleContents m
	| mkModule (modulePackageId mdl) m == mdl -> return localNames 
	| otherwise -> let m' = mkModule (modulePackageId mdl) m in
	  case Map.lookup m' modMap of
	    Just mod
		| OptHide `elem` ifaceOptions mod ->
		    return (filter (`elem` scope) (ifaceExports mod))
		| otherwise -> return []
	    Nothing
		-> tell (exportModuleMissingErr mdl m') >> return []
  
    _ -> return []


exportModuleMissingErr this mdl 
  = ["Warning: in export list of " ++ show (moduleString this)
	 ++ ": module not found: " ++ show (moduleString mdl)]


-- | For a given entity, find all the names it "owns" (ie. all the
-- constructors and field names of a tycon, or all the methods of a
-- class).
allSubsOfName :: ModuleMap -> Name -> [Name]
allSubsOfName modMap name 
  | isExternalName name =
    case Map.lookup (nameModule name) modMap of
      Just iface -> Map.findWithDefault [] name (ifaceSubMap iface)
      Nothing   -> []
  | otherwise =  error $ "Main.allSubsOfName: unexpected unqual'd name"


-- Named documentation

findNamedDoc :: String -> [HsDecl Name] -> ErrMsgM (Maybe (HsDoc Name))
findNamedDoc name decls = search decls
	where
    search [] = do
		  tell ["Cannot find documentation for: $" ++ name]
		  return Nothing
    search ((DocD (DocCommentNamed name' doc)):rest) 
      | name == name' = return (Just doc)
      | otherwise = search rest
    search (_other_decl : rest) = search rest
