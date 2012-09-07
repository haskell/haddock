{-# LANGUAGE CPP, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.AttachInstances
-- Copyright   :  (c) Simon Marlow 2006,
--                    David Waern  2006-2009,
--                    Isaac Dupree 2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Interface.AttachInstances (attachInstances) where


import Haddock.Types
import Haddock.Convert

import Control.Arrow
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import GHC
import Name
import InstEnv
import Class
import GhcMonad (withSession)
import TysPrim( funTyCon )
import MonadUtils (liftIO)
import TcRnDriver (tcRnGetInfo)
import TypeRep
import Var hiding (varName)
import TyCon
import PrelNames
import FastString
#define FSLIT(x) (mkFastString# (x#))

type ExportedNames = Set.Set Name
type Modules = Set.Set Module
type ExportInfo = (ExportedNames, Modules)

attachInstances :: ExportInfo -> [Interface] -> InstIfaceMap -> Ghc [Interface]
attachInstances expInfo ifaces instIfaceMap = mapM attach ifaces
  where
    -- TODO: take an IfaceMap as input
    ifaceMap = Map.fromList [ (ifaceMod i, i) | i <- ifaces ]

    attach iface = do
      newItems <- mapM (attachToExportItem expInfo iface ifaceMap instIfaceMap)
                       (ifaceExportItems iface)
      return $ iface { ifaceExportItems = newItems }


attachToExportItem :: ExportInfo -> Interface -> IfaceMap -> InstIfaceMap -> ExportItem Name -> Ghc (ExportItem Name)
attachToExportItem expInfo iface ifaceMap instIfaceMap export =
  case export of
    ExportDecl { expItemDecl = L _ (TyClD d) } -> do
      mb_info <- getAllInfo (unLoc (tcdLName d))
      let export' =
            export {
              expItemInstances =
                case mb_info of
                  Just (_, _, instances) ->
                    let insts = map (first synifyInstHead) $ sortImage (first instHead) $
                                filter (\((_,_,cls,tys),_) -> not $ isInstanceHidden expInfo cls tys)
                                [ (instanceHead i, getName i) | i <- instances ]
                    in [ (inst, lookupInstDoc name iface ifaceMap instIfaceMap)
                       | (inst, name) <- insts ]
                  Nothing -> []
            }
      return export'
    _ -> return export


lookupInstDoc :: Name -> Interface -> IfaceMap -> InstIfaceMap -> Maybe (Doc Name)
-- TODO: capture this pattern in a function (when we have streamlined the
-- handling of instances)
lookupInstDoc name iface ifaceMap instIfaceMap =
  case Map.lookup name (ifaceDocMap iface) of
    Just doc -> Just doc
    Nothing ->
      case Map.lookup modName ifaceMap of
        Just iface2 ->
          case Map.lookup name (ifaceDocMap iface2) of
            Just doc -> Just doc
            Nothing -> Nothing
        Nothing ->
          case Map.lookup modName instIfaceMap of
            Just instIface -> Map.lookup name (instDocMap instIface)
            Nothing -> Nothing
  where
    modName = nameModule name


-- | Like GHC's getInfo but doesn't cut things out depending on the
-- interative context, which we don't set sufficiently anyway.
getAllInfo :: GhcMonad m => Name -> m (Maybe (TyThing,Fixity,[ClsInst]))
getAllInfo name = withSession $ \hsc_env -> do 
   (_msgs, r) <- liftIO $ tcRnGetInfo hsc_env name
   return r


--------------------------------------------------------------------------------
-- Collecting and sorting instances
--------------------------------------------------------------------------------


-- | Simplified type for sorting types, ignoring qualification (not visible
-- in Haddock output) and unifying special tycons with normal ones.
-- For the benefit of the user (looks nice and predictable) and the
-- tests (which prefer output to be deterministic).
data SimpleType = SimpleType Name [SimpleType]
                | SimpleTyLit TyLit
                  deriving (Eq,Ord)


instHead :: ([TyVar], [PredType], Class, [Type]) -> ([Int], Name, [SimpleType])
instHead (_, _, cls, args)
  = (map argCount args, className cls, map simplify args)
  where
    argCount (AppTy t _) = argCount t + 1
    argCount (TyConApp _ ts) = length ts
    argCount (FunTy _ _ ) = 2
    argCount (ForAllTy _ t) = argCount t
    argCount _ = 0

    simplify (ForAllTy _ t) = simplify t
    simplify (FunTy t1 t2) = 
      SimpleType funTyConName [simplify t1, simplify t2]
    simplify (AppTy t1 t2) = SimpleType s (ts ++ [simplify t2])
      where (SimpleType s ts) = simplify t1
    simplify (TyVarTy v) = SimpleType (tyVarName v) []
    simplify (TyConApp tc ts) = SimpleType (tyConName tc) (map simplify ts)
    simplify (LitTy l) = SimpleTyLit l


-- sortImage f = sortBy (\x y -> compare (f x) (f y))
sortImage :: Ord b => (a -> b) -> [a] -> [a]
sortImage f xs = map snd $ sortBy cmp_fst [(f x, x) | x <- xs]
 where cmp_fst (x,_) (y,_) = compare x y


funTyConName :: Name
funTyConName = mkWiredInName gHC_PRIM
                        (mkOccNameFS tcName FSLIT("(->)"))
                        funTyConKey
                        (ATyCon funTyCon)       -- Relevant TyCon
                        BuiltInSyntax

--------------------------------------------------------------------------------
-- Filtering hidden instances
--------------------------------------------------------------------------------

-- | A class or data type is hidden iff
--
-- * it is defined in one of the modules that are being processed
--
-- * and it is not exported by any non-hidden module
isNameHidden :: ExportInfo -> Name -> Bool
isNameHidden (names, modules) name =
  nameModule name `Set.member` modules &&
  not (name `Set.member` names)

-- | We say that an instance is «hidden» iff its class or any (part)
-- of its type(s) is hidden.
isInstanceHidden :: ExportInfo -> Class -> [Type] -> Bool
isInstanceHidden expInfo cls tys =
    instClassHidden || instTypeHidden
  where
    instClassHidden :: Bool
    instClassHidden = isNameHidden expInfo $ getName cls

    instTypeHidden :: Bool
    instTypeHidden = any typeHidden tys

    nameHidden :: Name -> Bool
    nameHidden = isNameHidden expInfo

    typeHidden :: Type -> Bool
    typeHidden t =
      case t of
        TyVarTy {} -> False
        AppTy t1 t2 -> typeHidden t1 || typeHidden t2
        TyConApp tcon args -> nameHidden (getName tcon) || any typeHidden args
        FunTy t1 t2 -> typeHidden t1 || typeHidden t2
        ForAllTy _ ty -> typeHidden ty
        LitTy _ -> False
