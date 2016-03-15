{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Utils
-- Copyright   :  (c) The University of Glasgow 2001-2002,
--                    Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Utils (

  -- * Misc utilities
  restrictTo, emptyHsQTvs,
  toDescription, toInstalledDescription,
  mkEmptySigWcType, addClassContext, lHsQTyVarsToTypes,

  -- * Filename utilities
  moduleHtmlFile, moduleHtmlFile',
  contentsHtmlFile, indexHtmlFile,
  frameIndexHtmlFile,
  moduleIndexFrameName, mainFrameName, synopsisFrameName,
  subIndexHtmlFile,
  jsFile, framesFile,

  -- * Anchor and URL utilities
  moduleNameUrl, moduleNameUrl', moduleUrl,
  nameAnchorId,
  makeAnchorId,

  -- * Miscellaneous utilities
  getProgramName, bye, die, dieMsg, noDieMsg, mapSnd, mapMaybeM, escapeStr,

  -- * HTML cross reference mapping
  html_xrefs_ref, html_xrefs_ref',

  -- * Doc markup
  markup,
  idMarkup,
  mkMeta,

  -- * List utilities
  replace,
  spanWith,

  -- * MTL stuff
  MonadIO(..),

  -- * Logging
  parseVerbosity,
  out,

  -- * System tools
  getProcessID
 ) where


import Documentation.Haddock.Doc (emptyMetaDoc)
import Haddock.Types
import Haddock.GhcUtils

import GHC
import Name
import NameSet ( emptyNameSet )
import HsTypes (selectorFieldOcc)

import Control.Monad ( liftM )
import Data.Char ( isAlpha, isAlphaNum, isAscii, ord, chr )
import Numeric ( showIntAtBase )
import Data.Map ( Map )
import qualified Data.Map as Map hiding ( Map )
import Data.IORef ( IORef, newIORef, readIORef )
import Data.List ( isSuffixOf )
import Data.Maybe ( mapMaybe )
import System.Environment ( getProgName )
import System.Exit
import System.IO ( hPutStr, stderr )
import System.IO.Unsafe ( unsafePerformIO )
import qualified System.FilePath.Posix as HtmlPath
import Distribution.Verbosity
import Distribution.ReadE

#ifndef mingw32_HOST_OS
import qualified System.Posix.Internals
#endif

import MonadUtils ( MonadIO(..) )


--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------


parseVerbosity :: String -> Either String Verbosity
parseVerbosity = runReadE flagToVerbosity


-- | Print a message to stdout, if it is not too verbose
out :: MonadIO m
    => Verbosity -- ^ program verbosity
    -> Verbosity -- ^ message verbosity
    -> String -> m ()
out progVerbosity msgVerbosity msg
  | msgVerbosity <= progVerbosity = liftIO $ putStrLn msg
  | otherwise = return ()


--------------------------------------------------------------------------------
-- * Some Utilities
--------------------------------------------------------------------------------


-- | Extract a module's short description.
toDescription :: Interface -> Maybe (MDoc Name)
toDescription = fmap mkMeta . hmi_description . ifaceInfo


-- | Extract a module's short description.
toInstalledDescription :: InstalledInterface -> Maybe (MDoc Name)
toInstalledDescription = fmap mkMeta . hmi_description . instInfo

mkMeta :: Doc a -> MDoc a
mkMeta x = emptyMetaDoc { _doc = x }

mkEmptySigWcType :: LHsType Name -> LHsSigWcType Name
-- Dubious, because the implicit binders are empty even
-- though the type might have free varaiables
mkEmptySigWcType ty = mkEmptyImplicitBndrs (mkEmptyWildCardBndrs ty)

addClassContext :: Name -> LHsQTyVars Name -> LSig Name -> LSig Name
-- Add the class context to a class-op signature
addClassContext cls tvs0 (L pos (ClassOpSig _ lname ltype))
  = L pos (TypeSig lname (mkEmptySigWcType (go (hsSigType ltype))))
          -- The mkEmptySigWcType is suspicious
  where
    go (L loc (HsForAllTy { hst_bndrs = tvs, hst_body = ty }))
       = L loc (HsForAllTy { hst_bndrs = tvs, hst_body = go ty })
    go (L loc (HsQualTy { hst_ctxt = ctxt, hst_body = ty }))
       = L loc (HsQualTy { hst_ctxt = add_ctxt ctxt, hst_body = ty })
    go (L loc ty)
       = L loc (HsQualTy { hst_ctxt = add_ctxt (L loc []), hst_body = L loc ty })

    extra_pred = nlHsTyConApp cls (lHsQTyVarsToTypes tvs0)
    add_ctxt (L loc preds) = L loc (extra_pred : preds)

addClassContext _ _ sig = sig   -- E.g. a MinimalSig is fine

lHsQTyVarsToTypes :: LHsQTyVars Name -> [LHsType Name]
lHsQTyVarsToTypes tvs
  = [ noLoc (HsTyVar (noLoc (hsLTyVarName tv)))
    | tv <- hsQTvExplicit tvs ]

--------------------------------------------------------------------------------
-- * Making abstract declarations
--------------------------------------------------------------------------------


restrictTo :: [Name] -> LHsDecl Name -> LHsDecl Name
restrictTo names (L loc decl) = L loc $ case decl of
  TyClD d | isDataDecl d  ->
    TyClD (d { tcdDataDefn = restrictDataDefn names (tcdDataDefn d) })
  TyClD d | isClassDecl d ->
    TyClD (d { tcdSigs = restrictDecls names (tcdSigs d),
               tcdATs = restrictATs names (tcdATs d) })
  _ -> decl

restrictDataDefn :: [Name] -> HsDataDefn Name -> HsDataDefn Name
restrictDataDefn names defn@(HsDataDefn { dd_ND = new_or_data, dd_cons = cons })
  | DataType <- new_or_data
  = defn { dd_cons = restrictCons names cons }
  | otherwise    -- Newtype
  = case restrictCons names cons of
      []    -> defn { dd_ND = DataType, dd_cons = [] }
      [con] -> defn { dd_cons = [con] }
      _ -> error "Should not happen"

restrictCons :: [Name] -> [LConDecl Name] -> [LConDecl Name]
restrictCons names decls = [ L p d | L p (Just d) <- map (fmap keep) decls ]
  where
    keep d | any (\n -> n `elem` names) (map unLoc $ getConNames d) =
      case getConDetails h98d of
        PrefixCon _ -> Just d
        RecCon fields
          | all field_avail (unL fields) -> Just d
          | otherwise -> Just (h98d { con_details = PrefixCon (field_types (map unL (unL fields))) })
          -- if we have *all* the field names available, then
          -- keep the record declaration.  Otherwise degrade to
          -- a constructor declaration.  This isn't quite right, but
          -- it's the best we can do.
        InfixCon _ _ -> Just d
      where
        h98d = h98ConDecl d
        h98ConDecl c@ConDeclH98{} = c
        h98ConDecl c@ConDeclGADT{} = c'
          where
            (details,_res_ty,cxt,tvs) = gadtDeclDetails (con_type c)
            c' :: ConDecl Name
            c' = ConDeclH98
                   { con_name = head (con_names c)
                   , con_qvars = Just $ HsQTvs { hsq_implicit = mempty
                                               , hsq_explicit = tvs
                                               , hsq_dependent = emptyNameSet }
                   , con_cxt = Just cxt
                   , con_details = details
                   , con_doc = con_doc c
                   }

        field_avail :: LConDeclField Name -> Bool
        field_avail (L _ (ConDeclField fs _ _))
            = all (\f -> selectorFieldOcc (unLoc f) `elem` names) fs
        field_types flds = [ t | ConDeclField _ t _ <- flds ]

    keep _ = Nothing

restrictDecls :: [Name] -> [LSig Name] -> [LSig Name]
restrictDecls names = mapMaybe (filterLSigNames (`elem` names))


restrictATs :: [Name] -> [LFamilyDecl Name] -> [LFamilyDecl Name]
restrictATs names ats = [ at | at <- ats , unL (fdLName (unL at)) `elem` names ]

emptyHsQTvs :: LHsQTyVars Name
-- This function is here, rather than in HsTypes, because it *renamed*, but
-- does not necessarily have all the rigt kind variables.  It is used
-- in Haddock just for printing, so it doesn't matter
emptyHsQTvs = HsQTvs { hsq_implicit = error "haddock:emptyHsQTvs"
                     , hsq_explicit = []
                     , hsq_dependent = error "haddock:emptyHsQTvs" }


--------------------------------------------------------------------------------
-- * Filename mangling functions stolen from s main/DriverUtil.lhs.
--------------------------------------------------------------------------------


baseName :: ModuleName -> FilePath
baseName = map (\c -> if c == '.' then '-' else c) . moduleNameString


moduleHtmlFile :: Module -> FilePath
moduleHtmlFile mdl =
  case Map.lookup mdl html_xrefs of
    Nothing  -> baseName mdl' ++ ".html"
    Just fp0 -> HtmlPath.joinPath [fp0, baseName mdl' ++ ".html"]
  where
   mdl' = moduleName mdl


moduleHtmlFile' :: ModuleName -> FilePath
moduleHtmlFile' mdl =
  case Map.lookup mdl html_xrefs' of
    Nothing  -> baseName mdl ++ ".html"
    Just fp0 -> HtmlPath.joinPath [fp0, baseName mdl ++ ".html"]


contentsHtmlFile, indexHtmlFile :: String
contentsHtmlFile = "index.html"
indexHtmlFile = "doc-index.html"


-- | The name of the module index file to be displayed inside a frame.
-- Modules are display in full, but without indentation.  Clicking opens in
-- the main window.
frameIndexHtmlFile :: String
frameIndexHtmlFile = "index-frames.html"


moduleIndexFrameName, mainFrameName, synopsisFrameName :: String
moduleIndexFrameName = "modules"
mainFrameName = "main"
synopsisFrameName = "synopsis"


subIndexHtmlFile :: String -> String
subIndexHtmlFile ls = "doc-index-" ++ b ++ ".html"
   where b | all isAlpha ls = ls
           | otherwise = concatMap (show . ord) ls


-------------------------------------------------------------------------------
-- * Anchor and URL utilities
--
-- NB: Anchor IDs, used as the destination of a link within a document must
-- conform to XML's NAME production. That, taken with XHTML and HTML 4.01's
-- various needs and compatibility constraints, means these IDs have to match:
--      [A-Za-z][A-Za-z0-9:_.-]*
-- Such IDs do not need to be escaped in any way when used as the fragment part
-- of a URL. Indeed, %-escaping them can lead to compatibility issues as it
-- isn't clear if such fragment identifiers should, or should not be unescaped
-- before being matched with IDs in the target document.
-------------------------------------------------------------------------------


moduleUrl :: Module -> String
moduleUrl = moduleHtmlFile


moduleNameUrl :: Module -> OccName -> String
moduleNameUrl mdl n = moduleUrl mdl ++ '#' : nameAnchorId n


moduleNameUrl' :: ModuleName -> OccName -> String
moduleNameUrl' mdl n = moduleHtmlFile' mdl ++ '#' : nameAnchorId n


nameAnchorId :: OccName -> String
nameAnchorId name = makeAnchorId (prefix : ':' : occNameString name)
 where prefix | isValOcc name = 'v'
              | otherwise     = 't'


-- | Takes an arbitrary string and makes it a valid anchor ID. The mapping is
-- identity preserving.
makeAnchorId :: String -> String
makeAnchorId [] = []
makeAnchorId (f:r) = escape isAlpha f ++ concatMap (escape isLegal) r
  where
    escape p c | p c = [c]
               | otherwise = '-' : show (ord c) ++ "-"
    isLegal ':' = True
    isLegal '_' = True
    isLegal '.' = True
    isLegal c = isAscii c && isAlphaNum c
       -- NB: '-' is legal in IDs, but we use it as the escape char


-------------------------------------------------------------------------------
-- * Files we need to copy from our $libdir
-------------------------------------------------------------------------------


jsFile, framesFile :: String
jsFile    = "haddock-util.js"
framesFile = "frames.html"


-------------------------------------------------------------------------------
-- * Misc.
-------------------------------------------------------------------------------


getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str


bye :: String -> IO a
bye s = putStr s >> exitSuccess


dieMsg :: String -> IO ()
dieMsg s = getProgramName >>= \prog -> die (prog ++ ": " ++ s)


noDieMsg :: String -> IO ()
noDieMsg s = getProgramName >>= \prog -> hPutStr stderr (prog ++ ": " ++ s)


mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd _ [] = []
mapSnd f ((x,y):xs) = (x,f y) : mapSnd f xs


mapMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMaybeM _ Nothing = return Nothing
mapMaybeM f (Just a) = liftM Just (f a)


escapeStr :: String -> String
escapeStr = escapeURIString isUnreserved


-- Following few functions are copy'n'pasted from Network.URI module
-- to avoid depending on the network lib, since doing so gives a
-- circular build dependency between haddock and network
-- (at least if you want to build network with haddock docs)
escapeURIChar :: (Char -> Bool) -> Char -> String
escapeURIChar p c
    | p c       = [c]
    | otherwise = '%' : myShowHex (ord c) ""
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 toChrHex n r of
            []  -> "00"
            [a] -> ['0',a]
            cs  -> cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))


escapeURIString :: (Char -> Bool) -> String -> String
escapeURIString = concatMap . escapeURIChar


isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNumChar c || (c `elem` "-_.~")


isAlphaChar, isDigitChar, isAlphaNumChar :: Char -> Bool
isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
isDigitChar c    = c >= '0' && c <= '9'
isAlphaNumChar c = isAlphaChar c || isDigitChar c


-----------------------------------------------------------------------------
-- * HTML cross references
--
-- For each module, we need to know where its HTML documentation lives
-- so that we can point hyperlinks to it.  It is extremely
-- inconvenient to plumb this information to all the places that need
-- it (basically every function in HaddockHtml), and furthermore the
-- mapping is constant for any single run of Haddock.  So for the time
-- being I'm going to use a write-once global variable.
-----------------------------------------------------------------------------


{-# NOINLINE html_xrefs_ref #-}
html_xrefs_ref :: IORef (Map Module FilePath)
html_xrefs_ref = unsafePerformIO (newIORef (error "module_map"))


{-# NOINLINE html_xrefs_ref' #-}
html_xrefs_ref' :: IORef (Map ModuleName FilePath)
html_xrefs_ref' = unsafePerformIO (newIORef (error "module_map"))


{-# NOINLINE html_xrefs #-}
html_xrefs :: Map Module FilePath
html_xrefs = unsafePerformIO (readIORef html_xrefs_ref)


{-# NOINLINE html_xrefs' #-}
html_xrefs' :: Map ModuleName FilePath
html_xrefs' = unsafePerformIO (readIORef html_xrefs_ref')


-----------------------------------------------------------------------------
-- * List utils
-----------------------------------------------------------------------------


replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)


spanWith :: (a -> Maybe b) -> [a] -> ([b],[a])
spanWith _ [] = ([],[])
spanWith p xs@(a:as)
  | Just b <- p a = let (bs,cs) = spanWith p as in (b:bs,cs)
  | otherwise     = ([],xs)


-----------------------------------------------------------------------------
-- * Put here temporarily
-----------------------------------------------------------------------------


markup :: DocMarkup id a -> Doc id -> a
markup m DocEmpty                    = markupEmpty m
markup m (DocAppend d1 d2)           = markupAppend m (markup m d1) (markup m d2)
markup m (DocString s)               = markupString m s
markup m (DocParagraph d)            = markupParagraph m (markup m d)
markup m (DocIdentifier x)           = markupIdentifier m x
markup m (DocIdentifierUnchecked x)  = markupIdentifierUnchecked m x
markup m (DocModule mod0)            = markupModule m mod0
markup m (DocWarning d)              = markupWarning m (markup m d)
markup m (DocEmphasis d)             = markupEmphasis m (markup m d)
markup m (DocBold d)                 = markupBold m (markup m d)
markup m (DocMonospaced d)           = markupMonospaced m (markup m d)
markup m (DocUnorderedList ds)       = markupUnorderedList m (map (markup m) ds)
markup m (DocOrderedList ds)         = markupOrderedList m (map (markup m) ds)
markup m (DocDefList ds)             = markupDefList m (map (markupPair m) ds)
markup m (DocCodeBlock d)            = markupCodeBlock m (markup m d)
markup m (DocHyperlink l)            = markupHyperlink m l
markup m (DocAName ref)              = markupAName m ref
markup m (DocPic img)                = markupPic m img
markup m (DocMathInline mathjax)     = markupMathInline m mathjax
markup m (DocMathDisplay mathjax)    = markupMathDisplay m mathjax
markup m (DocProperty p)             = markupProperty m p
markup m (DocExamples e)             = markupExample m e
markup m (DocHeader (Header l t))    = markupHeader m (Header l (markup m t))


markupPair :: DocMarkup id a -> (Doc id, Doc id) -> (a, a)
markupPair m (a,b) = (markup m a, markup m b)


-- | The identity markup
idMarkup :: DocMarkup a (Doc a)
idMarkup = Markup {
  markupEmpty                = DocEmpty,
  markupString               = DocString,
  markupParagraph            = DocParagraph,
  markupAppend               = DocAppend,
  markupIdentifier           = DocIdentifier,
  markupIdentifierUnchecked  = DocIdentifierUnchecked,
  markupModule               = DocModule,
  markupWarning              = DocWarning,
  markupEmphasis             = DocEmphasis,
  markupBold                 = DocBold,
  markupMonospaced           = DocMonospaced,
  markupUnorderedList        = DocUnorderedList,
  markupOrderedList          = DocOrderedList,
  markupDefList              = DocDefList,
  markupCodeBlock            = DocCodeBlock,
  markupHyperlink            = DocHyperlink,
  markupAName                = DocAName,
  markupPic                  = DocPic,
  markupMathInline           = DocMathInline,
  markupMathDisplay          = DocMathDisplay,
  markupProperty             = DocProperty,
  markupExample              = DocExamples,
  markupHeader               = DocHeader
  }


-----------------------------------------------------------------------------
-- * System tools
-----------------------------------------------------------------------------


#ifdef mingw32_HOST_OS
foreign import ccall unsafe "_getpid" getProcessID :: IO Int -- relies on Int == Int32 on Windows
#else
getProcessID :: IO Int
getProcessID = fmap fromIntegral System.Posix.Internals.c_getpid
#endif
