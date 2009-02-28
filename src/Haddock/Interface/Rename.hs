--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Interface.Rename (renameInterface) where


import Haddock.DocName
import Haddock.Types
import Haddock.GHC.Utils

import GHC hiding (NoLink)
import Name
import BasicTypes
import SrcLoc 
import Bag (emptyBag)
import Outputable
import Util (thenCmp)

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map hiding ( Map )
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Arrow
import Control.Monad hiding (mapM)


renameInterface :: LinkEnv -> Bool -> Interface -> ErrMsgM Interface
renameInterface renamingEnv warnings mod =

  -- first create the local env, where every name exported by this module
  -- is mapped to itself, and everything else comes from the global renaming
  -- env
  let localEnv = foldl fn renamingEnv (ifaceVisibleExports mod)
        where fn env name = Map.insert name (ifaceMod mod) env
      
      docMap = Map.map (\(_,x,_) -> x) (ifaceDeclMap mod)
      docs   = [ (n, doc) | (n, Just doc) <- Map.toList docMap ]
      renameMapElem (k,d) = do d' <- renameDoc d; return (k, d') 

      -- rename names in the exported declarations to point to things that
      -- are closer to, or maybe even exported by, the current module.
      (renamedExportItems, missingNames1)
        = runRnFM localEnv (renameExportItems (ifaceExportItems mod))

      (rnDocMap, missingNames2) 
        = runRnFM localEnv (liftM Map.fromList (mapM renameMapElem docs))

      (finalModuleDoc, missingNames3)
        = runRnFM localEnv (renameMaybeDoc (ifaceDoc mod))

      -- combine the missing names and filter out the built-ins, which would
      -- otherwise allways be missing. 
      missingNames = nub $ filter isExternalName
                    (missingNames1 ++ missingNames2 ++ missingNames3)

      -- filter out certain built in type constructors using their string 
      -- representation. TODO: use the Name constants from the GHC API.
--      strings = filter (`notElem` ["()", "[]", "(->)"]) 
--                (map pretty missingNames)
      strings = map pretty . filter (\n -> not (isSystemName n || isBuiltInSyntax n)) $ missingNames
     
  in do
    -- report things that we couldn't link to. Only do this for non-hidden
    -- modules.
    unless (OptHide `elem` ifaceOptions mod || null strings || not warnings) $
      tell ["Warning: " ++ moduleString (ifaceMod mod) ++
            ": could not find link destinations for:\n"++
            "   " ++ concat (map (' ':) strings) ]

    return $ mod { ifaceRnDoc = finalModuleDoc,
                   ifaceRnDocMap = rnDocMap,
                   ifaceRnExportItems = renamedExportItems }


--------------------------------------------------------------------------------
-- Monad for renaming
--
-- The monad does two things for us: it passes around the environment for
-- renaming, and it returns a list of names which couldn't be found in 
-- the environment.
--------------------------------------------------------------------------------


newtype GenRnM n a = 
  RnM { unRn :: (n -> (Bool, DocName))	-- name lookup function
             -> (a,[n])
      }

type RnM a = GenRnM Name a

instance Monad (GenRnM n) where
  (>>=) = thenRn
  return = returnRn   

returnRn :: a -> GenRnM n a
returnRn a   = RnM (\_ -> (a,[]))
thenRn :: GenRnM n a -> (a -> GenRnM n b) -> GenRnM n b
m `thenRn` k = RnM (\lkp -> case unRn m lkp of 
				(a,out1) -> case unRn (k a) lkp of
						(b,out2) -> (b,out1++out2))

getLookupRn :: RnM (Name -> (Bool, DocName))
getLookupRn = RnM (\lkp -> (lkp,[]))
outRn :: Name -> RnM ()
outRn name = RnM (\_ -> ((),[name]))

lookupRn :: (DocName -> a) -> Name -> RnM a
lookupRn and_then name = do
  lkp <- getLookupRn
  case lkp name of
	(False,maps_to) -> do outRn name; return (and_then maps_to)
	(True, maps_to) -> return (and_then maps_to)


runRnFM :: LinkEnv -> RnM a -> (a,[Name])
runRnFM env rn = unRn rn lkp 
  where 
    lkp n = case Map.lookup n env of
      Nothing  -> (False, Undocumented n) 
      Just mod -> (True,  Documented n mod)


--------------------------------------------------------------------------------
-- Renaming
--------------------------------------------------------------------------------


rename = lookupRn id 
renameL (L loc name) = return . L loc =<< rename name


renameExportItems :: [ExportItem Name] -> RnM [ExportItem DocName]
renameExportItems items = mapM renameExportItem items


renameMaybeDoc :: Maybe (HsDoc Name) -> RnM (Maybe (HsDoc DocName))
renameMaybeDoc mbDoc = mapM renameDoc mbDoc


renameLDoc (L loc doc) = return . L loc =<< renameDoc doc


renameDoc :: HsDoc Name -> RnM (HsDoc DocName)
renameDoc doc = case doc of
  DocEmpty -> return DocEmpty
  DocAppend a b -> do
    a' <- renameDoc a
    b' <- renameDoc b
    return (DocAppend a' b')
  DocString str -> return (DocString str)
  DocParagraph doc -> do
    doc' <- renameDoc doc
    return (DocParagraph doc')
  DocIdentifier ids -> do
    lkp <- getLookupRn
    case [ n | (True, n) <- map lkp ids ] of
      ids'@(_:_) -> return (DocIdentifier ids')
      [] -> return (DocIdentifier (map Undocumented ids))
  DocModule str -> return (DocModule str)
  DocEmphasis doc -> do
    doc' <- renameDoc doc
    return (DocEmphasis doc')
  DocMonospaced doc -> do
    doc' <- renameDoc doc
    return (DocMonospaced doc')
  DocUnorderedList docs -> do
    docs' <- mapM renameDoc docs
    return (DocUnorderedList docs')
  DocOrderedList docs -> do
    docs' <- mapM renameDoc docs
    return (DocOrderedList docs')
  DocDefList docs -> do
    docs' <- mapM (\(a,b) -> do
      a' <- renameDoc a
      b' <- renameDoc b
      return (a',b')) docs
    return (DocDefList docs')  
  DocCodeBlock doc -> do
    doc' <- renameDoc doc
    return (DocCodeBlock doc')
  DocURL str -> return (DocURL str) 
  DocPic str -> return (DocPic str)
  DocAName str -> return (DocAName str)


renameLPred (L loc p) = return . L loc =<< renamePred p


renamePred :: HsPred Name -> RnM (HsPred DocName)
renamePred (HsClassP name types) = do
  name'  <- rename name 
  types' <- mapM renameLType types
  return (HsClassP name' types')
renamePred (HsEqualP type1 type2) = do
  type1' <- renameLType type1
  type2' <- renameLType type2
  return (HsEqualP type1' type2')
renamePred (HsIParam (IPName name) t) = do
  name' <- rename name
  t'    <- renameLType t
  return (HsIParam (IPName name') t')


renameLType (L loc t) = return . L loc =<< renameType t


renameType t = case t of 
  HsForAllTy expl tyvars lcontext ltype -> do
    tyvars'   <- mapM renameLTyVarBndr tyvars
    lcontext' <- renameLContext lcontext 
    ltype'    <- renameLType ltype
    return (HsForAllTy expl tyvars' lcontext' ltype')

  HsTyVar n -> return . HsTyVar =<< rename n
  HsBangTy b ltype -> return . HsBangTy b =<< renameLType ltype
  
  HsAppTy a b -> do
    a' <- renameLType a
    b' <- renameLType b
    return (HsAppTy a' b')

  HsFunTy a b -> do     
    a' <- renameLType a
    b' <- renameLType b
    return (HsFunTy a' b')

  HsListTy t -> return . HsListTy =<< renameLType t
  HsPArrTy t -> return . HsPArrTy =<< renameLType t

  HsTupleTy b ts -> return . HsTupleTy b =<< mapM renameLType ts

  HsOpTy a (L loc op) b -> do
    op' <- rename op
    a'  <- renameLType a
    b'  <- renameLType b
    return (HsOpTy a' (L loc op') b')

  HsParTy t -> return . HsParTy =<< renameLType t

  HsNumTy n -> return (HsNumTy n)

  HsPredTy p -> return . HsPredTy =<< renamePred p

  HsKindSig t k -> do
    t' <- renameLType t
    return (HsKindSig t' k)

  HsDocTy t doc -> do
    t' <- renameLType t
    doc' <- renameLDoc doc
    return (HsDocTy t' doc')

  _ -> error "renameType"


renameLTyVarBndr (L loc tv) = do
  name' <- rename (hsTyVarName tv)
  return $ L loc (replaceTyVarName tv name')

    
renameLContext (L loc context) = do
  context' <- mapM renameLPred context
  return (L loc context')


renameInstHead :: InstHead Name -> RnM (InstHead DocName)
renameInstHead (preds, className, types) = do
  preds' <- mapM renamePred preds
  className' <- rename className
  types' <- mapM renameType types
  return (preds', className', types')


renameLDecl (L loc d) = return . L loc =<< renameDecl d


renameDecl d = case d of
  TyClD d -> do
    d' <- renameTyClD d
    return (TyClD d')
  SigD s -> do
    s' <- renameSig s
    return (SigD s')
  ForD d -> do
    d' <- renameForD d
    return (ForD d')
  InstD d -> do
    d' <- renameInstD d
    return (InstD d')
  _ -> error "renameDecl"


renameLTyClD (L loc d) = return . L loc =<< renameTyClD d 


renameTyClD d = case d of
  ForeignType lname a b -> do
    lname' <- renameL lname
    return (ForeignType lname' a b)

  TyFamily flav lname ltyvars kind -> do
    lname'   <- renameL lname
    ltyvars' <- mapM renameLTyVarBndr ltyvars
    return (TyFamily flav lname' ltyvars' kind)

  TyData x lcontext lname ltyvars typats k cons _ -> do
    lcontext' <- renameLContext lcontext
    lname'    <- renameL lname
    ltyvars'  <- mapM renameLTyVarBndr ltyvars
    typats'   <- mapM (mapM renameLType) typats
    cons'     <- mapM renameLCon cons
    -- I don't think we need the derivings, so we return Nothing
    return (TyData x lcontext' lname' ltyvars' typats' k cons' Nothing) 
 
  TySynonym lname ltyvars typats ltype -> do
    lname'   <- renameL lname
    ltyvars' <- mapM renameLTyVarBndr ltyvars
    ltype'   <- renameLType ltype
    typats'  <- mapM (mapM renameLType) typats
    return (TySynonym lname' ltyvars' typats' ltype')

  ClassDecl lcontext lname ltyvars lfundeps lsigs _ ats _ -> do
    lcontext' <- renameLContext lcontext
    lname'    <- renameL lname
    ltyvars'  <- mapM renameLTyVarBndr ltyvars
    lfundeps' <- mapM renameLFunDep lfundeps 
    lsigs'    <- mapM renameLSig lsigs
    ats'      <- mapM renameLTyClD ats
    -- we don't need the default methods or the already collected doc entities
    return (ClassDecl lcontext' lname' ltyvars' lfundeps' lsigs' emptyBag ats' [])
 
  where
    renameLCon (L loc con) = return . L loc =<< renameCon con
    renameCon (ConDecl lname expl ltyvars lcontext details restype mbldoc) = do
      lname'    <- renameL lname
      ltyvars'  <- mapM renameLTyVarBndr ltyvars
      lcontext' <- renameLContext lcontext
      details'  <- renameDetails details
      restype'  <- renameResType restype
      mbldoc'   <- mapM renameLDoc mbldoc
      return (ConDecl lname' expl ltyvars' lcontext' details' restype' mbldoc') 

    renameDetails (RecCon fields) = return . RecCon =<< mapM renameField fields
    renameDetails (PrefixCon ps) = return . PrefixCon =<< mapM renameLType ps
    renameDetails (InfixCon a b) = do
      a' <- renameLType a
      b' <- renameLType b
      return (InfixCon a' b')

    renameField (ConDeclField name t doc) = do
      name' <- renameL name
      t'   <- renameLType t
      doc' <- mapM renameLDoc doc
      return (ConDeclField name' t' doc')

    renameResType (ResTyH98) = return ResTyH98
    renameResType (ResTyGADT t) = return . ResTyGADT =<< renameLType t

    renameLFunDep (L loc (xs, ys)) = do
      xs' <- mapM rename xs
      ys' <- mapM rename ys
      return (L loc (xs', ys'))
   
    renameLSig (L loc sig) = return . L loc =<< renameSig sig

      
renameSig sig = case sig of 
  TypeSig lname ltype -> do 
    lname' <- renameL lname
    ltype' <- renameLType ltype
    return (TypeSig lname' ltype')
  -- we have filtered out all other kinds of signatures in Interface.Create


renameForD (ForeignImport lname ltype x) = do
  lname' <- renameL lname
  ltype' <- renameLType ltype
  return (ForeignImport lname' ltype' x)
renameForD (ForeignExport lname ltype x) = do
  lname' <- renameL lname
  ltype' <- renameLType ltype
  return (ForeignExport lname' ltype' x)


renameInstD (InstDecl ltype _ _ lATs) = do
  ltype <- renameLType ltype
  lATs' <- mapM renameLTyClD lATs
  return (InstDecl ltype emptyBag [] lATs') 


renameExportItem :: ExportItem Name -> RnM (ExportItem DocName)
renameExportItem item = case item of 
  ExportModule mod -> return (ExportModule mod)
  ExportGroup lev id doc -> do
    doc' <- renameDoc doc
    return (ExportGroup lev id doc')
  ExportDecl decl doc subs instances -> do
    decl' <- renameLDecl decl
    doc'  <- mapM renameDoc doc
    subs' <- mapM renameSub subs
    instances' <- mapM renameInstHead instances
    return (ExportDecl decl' doc' subs' instances')
  ExportNoDecl x subs -> do
    x'    <- lookupRn id x
    subs' <- mapM (lookupRn id) subs
    return (ExportNoDecl x' subs')
  ExportDoc doc -> do
    doc' <- renameDoc doc
    return (ExportDoc doc')


renameSub (n,doc) = do
  n' <- rename n
  doc' <- mapM renameDoc doc
  return (n', doc')
