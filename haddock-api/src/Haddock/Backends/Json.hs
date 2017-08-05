{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Json
-- Copyright   :  (c) Neil Mitchell 2006-2008
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Write out Json Module summaries
-----------------------------------------------------------------------------
module Haddock.Backends.Json (
    ppJson
  ) where

import Haddock.GhcUtils
import Haddock.Types hiding (Version)
import Haddock.Backends.Json.Json

import GHC
import Outputable

import Data.Char
import Data.List
import Data.Version

import System.Directory
import System.FilePath
import System.IO

ppJson :: DynFlags -> String -> Version -> String -> Maybe (Doc RdrName) -> [Interface] -> FilePath -> IO ()
--ppJson dflags package version synopsis prologue ifaces odir = do
ppJson dflags package _ _ _ ifaces odir = do
    let filename = package ++ ".json"
        contents = JSArray [ppModule dflags i | i <- ifaces, OptHide `notElem` ifaceOptions i]
    createDirectoryIfMissing True odir
    h <- openFile (odir </> filename) WriteMode
    hSetEncoding h utf8
    hPutStr h (showSDoc dflags (renderJSON contents))
    hClose h

ppModule :: DynFlags -> Interface -> JsonDoc
ppModule dflags iface = JSArray (concatMap (ppExport dflags) (ifaceExportItems iface))

ppExport :: DynFlags -> ExportItem Name -> [JsonDoc]
ppExport dflags ExportDecl { expItemDecl    = L _ decl
                           , expItemMbDoc   = (_, _)
                           , expItemSubDocs = _
                           , expItemFixities = _
                           } = f decl
    where
        f (SigD sig) = ppSig dflags sig
        f _ = []
ppExport _ _ = []

ppSigWithDoc :: DynFlags -> Sig Name -> [(Name, DocForDecl Name)] -> [JsonDoc]
ppSigWithDoc dflags (TypeSig names sig) _ = [pp_sig dflags names (hsSigWcType sig)]
ppSigWithDoc _ _ _ = []

ppSig :: DynFlags -> Sig Name -> [JsonDoc]
ppSig dflags x  = ppSigWithDoc dflags x []

pp_sig :: DynFlags -> [Located Name] -> LHsType Name -> JsonDoc
pp_sig dflags names (L _ typ)  = JSObject xs
       where xs = [("type",JSString "funtion"),
                   ("name",JSString prettyNames),
                   ("sig",JSString (outHsType dflags typ))]
             prettyNames = intercalate ", " $ map (out dflags) names

outHsType :: OutputableBndr a => DynFlags -> HsType a -> String
outHsType dflags = out dflags . dropHsDocTy


outWith :: Outputable a => (SDoc -> String) -> a -> [Char]
outWith p = f . unwords . map (dropWhile isSpace) . lines . p . ppr
    where
        f xs | " <document comment>" `isPrefixOf` xs = f $ drop 19 xs
        f (x:xs) = x : f xs
        f [] = []

out :: Outputable a => DynFlags -> a -> String
out dflags = outWith $ showSDocUnqual dflags


dropHsDocTy :: HsType a -> HsType a
dropHsDocTy = f
    where
        g (L src x) = L src (f x)
        f (HsForAllTy a e) = HsForAllTy a (g e)
        f (HsQualTy a e) = HsQualTy a (g e)
        f (HsBangTy a b) = HsBangTy a (g b)
        f (HsAppTy a b) = HsAppTy (g a) (g b)
        f (HsFunTy a b) = HsFunTy (g a) (g b)
        f (HsListTy a) = HsListTy (g a)
        f (HsPArrTy a) = HsPArrTy (g a)
        f (HsTupleTy a b) = HsTupleTy a (map g b)
        f (HsOpTy a b c) = HsOpTy (g a) b (g c)
        f (HsParTy a) = HsParTy (g a)
        f (HsKindSig a b) = HsKindSig (g a) b
        f (HsDocTy a _) = f $ unL a
        f x = x