{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.GhcUtils
-- Copyright   :  (c) David Waern 2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utils for dealing with types from the GHC API
-----------------------------------------------------------------------------
module Haddock.GhcUtils where


import Data.Version
import Control.Applicative  ( (<$>) )
import Control.Arrow
import Data.Foldable hiding (concatMap)
import Data.Traversable
import Distribution.Compat.ReadP
import Distribution.Text

import Exception
import Outputable
import Name
import Packages
import Module
import RdrName (GlobalRdrEnv)
import GhcMonad (withSession)
import HscTypes
import UniqFM
import GHC


moduleString :: Module -> String
moduleString = moduleNameString . moduleName


-- return the (name,version) of the package
modulePackageInfo :: Module -> (String, [Char])
modulePackageInfo modu = case unpackPackageId pkg of
                          Nothing -> (packageIdString pkg, "")
                          Just x -> (display $ pkgName x, showVersion (pkgVersion x))
  where pkg = modulePackageId modu


-- This was removed from GHC 6.11
-- XXX we shouldn't be using it, probably

-- | Try and interpret a GHC 'PackageId' as a cabal 'PackageIdentifer'. Returns @Nothing@ if
-- we could not parse it as such an object.
unpackPackageId :: PackageId -> Maybe PackageIdentifier
unpackPackageId p
  = case [ pid | (pid,"") <- readP_to_S parse str ] of
        []      -> Nothing
        (pid:_) -> Just pid
  where str = packageIdString p


mkModuleNoPackage :: String -> Module
mkModuleNoPackage str = mkModule (stringToPackageId "") (mkModuleName str)


lookupLoadedHomeModuleGRE  :: GhcMonad m => ModuleName -> m (Maybe GlobalRdrEnv)
lookupLoadedHomeModuleGRE mod_name = withSession $ \hsc_env ->
  case lookupUFM (hsc_HPT hsc_env) mod_name of
    Just mod_info      -> return (mi_globals (hm_iface mod_info))
    _not_a_home_module -> return Nothing


isNameSym :: Name -> Bool
isNameSym = isSymOcc . nameOccName


isVarSym :: OccName -> Bool
isVarSym = isLexVarSym . occNameFS


getMainDeclBinder :: HsDecl name -> [name]
getMainDeclBinder (TyClD d) = [tcdName d]
getMainDeclBinder (ValD d) =
  case collectHsBindBinders d of
    []       -> []
    (name:_) -> [name]
getMainDeclBinder (SigD d) = sigNameNoLoc d
getMainDeclBinder (ForD (ForeignImport name _ _ _)) = [unLoc name]
getMainDeclBinder (ForD (ForeignExport _ _ _ _)) = []
getMainDeclBinder _ = []


-- Useful when there is a signature with multiple names, e.g.
--   foo, bar :: Types..
-- but only one of the names is exported and we have to change the
-- type signature to only include the exported names.
filterLSigNames :: (name -> Bool) -> LSig name -> Maybe (LSig name)
filterLSigNames p (L loc sig) = L loc <$> (filterSigNames p sig)

filterSigNames :: (name -> Bool) -> Sig name -> Maybe (Sig name)
filterSigNames p orig@(SpecSig n _ _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p orig@(InlineSig n _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p orig@(FixSig (FixitySig n _)) = ifTrueJust (p $ unLoc n) orig
filterSigNames p (TypeSig ns ty)               =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (TypeSig filtered ty)
filterSigNames _ _                           = Nothing

ifTrueJust :: Bool -> name -> Maybe name
ifTrueJust True  = Just
ifTrueJust False = const Nothing

sigName :: LSig name -> [name]
sigName (L _ sig) = sigNameNoLoc sig

sigNameNoLoc :: Sig name -> [name]
sigNameNoLoc (TypeSig   ns _)         = map unLoc ns
sigNameNoLoc (SpecSig   n _ _)        = [unLoc n]
sigNameNoLoc (InlineSig n _)          = [unLoc n]
sigNameNoLoc (FixSig (FixitySig n _)) = [unLoc n]
sigNameNoLoc _                        = []


isTyClD :: HsDecl a -> Bool
isTyClD (TyClD _) = True
isTyClD _ = False


isClassD :: HsDecl a -> Bool
isClassD (TyClD d) = isClassDecl d
isClassD _ = False


isDocD :: HsDecl a -> Bool
isDocD (DocD _) = True
isDocD _ = False


isInstD :: HsDecl a -> Bool
isInstD (InstD _) = True
isInstD (TyClD d) = isFamInstDecl d
isInstD _ = False


declATs :: HsDecl a -> [a]
declATs (TyClD d) | isClassDecl d = map (tcdName . unL) $ tcdATs d
declATs _ = []


pretty :: Outputable a => a -> String
pretty x = showSDoc (ppr x)


trace_ppr :: Outputable a => a -> b -> b
trace_ppr x y = trace (pretty x) y


-------------------------------------------------------------------------------
-- * Located
-------------------------------------------------------------------------------


unL :: Located a -> a
unL (L _ x) = x


reL :: a -> Located a
reL = L undefined


instance Foldable (GenLocated l) where
  foldMap f (L _ x) = f x


instance Traversable (GenLocated l) where
  mapM f (L l x) = (return . L l) =<< f x


-------------------------------------------------------------------------------
-- * NamedThing instances
-------------------------------------------------------------------------------


instance NamedThing (TyClDecl Name) where
  getName = tcdName


instance NamedThing (ConDecl Name) where
  getName = unL . con_name


-------------------------------------------------------------------------------
-- * Subordinates
-------------------------------------------------------------------------------


class Parent a where
  children :: a -> [Name]


instance Parent (ConDecl Name) where
  children con =
    case con_details con of
      RecCon fields -> map (unL . cd_fld_name) fields
      _             -> []


instance Parent (TyClDecl Name) where
  children d
    | isDataDecl  d = map (unL . con_name . unL) . tcdCons $ d
    | isClassDecl d =
        map (tcdName . unL) (tcdATs d) ++
        [ unL n | L _ (TypeSig ns _) <- tcdSigs d, n <- ns ]
    | otherwise = []


-- | A parent and its children
family :: (NamedThing a, Parent a) => a -> (Name, [Name])
family = getName &&& children


-- | A mapping from the parent (main-binder) to its children and from each
-- child to its grand-children, recursively.
families :: TyClDecl Name -> [(Name, [Name])]
families d
  | isDataDecl  d = family d : map (family . unL) (tcdCons d)
  | isClassDecl d = family d : concatMap (families . unL) (tcdATs d)
  | otherwise     = []


-- | A mapping from child to parent
parentMap :: TyClDecl Name -> [(Name, Name)]
parentMap d = [ (c, p) | (p, cs) <- families d, c <- cs ]


-- | The parents of a subordinate in a declaration
parents :: Name -> HsDecl Name -> [Name]
parents n (TyClD d) = [ p | (c, p) <- parentMap d, c == n ]
parents _ _ = []


-------------------------------------------------------------------------------
-- * Utils that work in monads defined by GHC
-------------------------------------------------------------------------------


modifySessionDynFlags :: (DynFlags -> DynFlags) -> Ghc ()
modifySessionDynFlags f = do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags (f dflags)
  return ()


-- | A variant of 'gbracket' where the return value from the first computation
-- is not required.
gbracket_ :: ExceptionMonad m => m a -> m b -> m c -> m c
gbracket_ before after thing = gbracket before (const after) (const thing)


-------------------------------------------------------------------------------
-- * DynFlags
-------------------------------------------------------------------------------


setObjectDir, setHiDir, setStubDir, setOutputDir :: String -> DynFlags -> DynFlags
setObjectDir  f d = d{ objectDir  = Just f}
setHiDir      f d = d{ hiDir      = Just f}
setStubDir    f d = d{ stubDir    = Just f, includePaths = f : includePaths d }
  -- -stubdir D adds an implicit -I D, so that gcc can find the _stub.h file
  -- \#included from the .hc file when compiling with -fvia-C.
setOutputDir  f = setObjectDir f . setHiDir f . setStubDir f

