--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}


module Haddock.GHC.Utils where


import Data.Char
import Data.Version
import qualified Data.Map as Map

import GHC
import HsSyn
import SrcLoc
import Outputable
import Name
import Packages


moduleString :: Module -> String
moduleString = moduleNameString . moduleName 


-- return the name of the package, with version info
modulePkgStr = packageIdString . modulePackageId


-- return the (name,version) of the package
modulePkgInfo mod = case unpackPackageId pkg of
                        Nothing -> (packageIdString pkg, "")
                        Just x -> (pkgName x, showVersion (pkgVersion x))
    where pkg = modulePackageId mod


mkModuleNoPkg :: String -> Module
mkModuleNoPkg str = mkModule (stringToPackageId "") (mkModuleName str)


instance (Outputable a, Outputable b) => Outputable (Map.Map a b) where
  ppr m = ppr (Map.toList m)


isNameSym :: Name -> Bool
isNameSym = isSymOcc . nameOccName


isVarSym :: OccName -> Bool
isVarSym = isLexVarSym . occNameFS


getMainDeclBinder :: HsDecl name -> Maybe name
getMainDeclBinder (TyClD d) = Just (tcdName d)
getMainDeclBinder (ValD d)
   = case collectAcc d [] of
        []       -> Nothing 
        (name:_) -> Just (unLoc name)
getMainDeclBinder (SigD d) = sigNameNoLoc d
getMainDeclBinder (ForD (ForeignImport name _ _)) = Just (unLoc name)
getMainDeclBinder (ForD (ForeignExport _ _ _)) = Nothing
getMainDeclBinder _ = Nothing


pretty :: Outputable a => a -> String
pretty x = show (ppr x defaultUserStyle)


trace_ppr :: Outputable a => a -> b -> b
trace_ppr x y = trace (showSDoc (ppr x)) y
