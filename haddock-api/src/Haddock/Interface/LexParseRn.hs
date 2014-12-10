{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE BangPatterns #-}
  -----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.LexParseRn
-- Copyright   :  (c) Isaac Dupree 2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Interface.LexParseRn
  ( processDocString
  , processDocStringParas
  , processDocStrings
  , processModuleHeader
  ) where

import Control.Applicative
import Data.IntSet (toList)
import Data.List
import Documentation.Haddock.Doc (metaDocConcat)
import DynFlags (ExtensionFlag(..), languageExtensions)
import FastString
import GHC
import Haddock.Interface.ParseModuleHeader
import Haddock.Parser
import Haddock.Types
import Name
import Outputable (showPpr)
import RdrName

processDocStrings :: DynFlags -> GlobalRdrEnv -> [HsDocString]
                  -> Maybe (MDoc Name)
processDocStrings dflags gre strs =
  case metaDocConcat $ map (processDocStringParas dflags gre) strs of
    -- We check that we don't have any version info to render instead
    -- of just checking if there is no comment: there may not be a
    -- comment but we still want to pass through any meta data.
    MetaDoc { _meta = Meta { _version = Nothing }, _doc = DocEmpty } -> Nothing
    x -> Just x

processDocStringParas :: DynFlags -> GlobalRdrEnv -> HsDocString -> MDoc Name
processDocStringParas dflags gre (HsDocString fs) =
  overDoc (rename dflags gre) $ parseParas dflags (unpackFS fs)

processDocString :: DynFlags -> GlobalRdrEnv -> HsDocString -> Doc Name
processDocString dflags gre (HsDocString fs) =
  rename dflags gre $ parseString dflags (unpackFS fs)

processModuleHeader :: DynFlags -> GlobalRdrEnv -> SafeHaskellMode -> Maybe LHsDocString
                    -> ErrMsgM (HaddockModInfo Name, Maybe (MDoc Name))
processModuleHeader dflags gre safety mayStr = do
  (hmi, doc) <-
    case mayStr of
      Nothing -> return failure
      Just (L _ (HsDocString fs)) -> do
        let str = unpackFS fs
            (hmi, doc) = parseModuleHeader dflags str
            !descr = rename dflags gre <$> hmi_description hmi
            hmi' = hmi { hmi_description = descr }
            doc' = overDoc (rename dflags gre) doc
        return (hmi', Just doc')

  let flags :: [ExtensionFlag]
      -- We remove the flags implied by the language setting and we display the language instead
      flags = map toEnum (toList $ extensionFlags dflags) \\ languageExtensions (language dflags)
  return (hmi { hmi_safety = Just $ showPpr dflags safety
              , hmi_language = language dflags
              , hmi_extensions = flags
              } , doc)
  where
    failure = (emptyHaddockModInfo, Nothing)


rename :: DynFlags -> GlobalRdrEnv -> Doc RdrName -> Doc Name
rename dflags gre = rn
  where
    rn d = case d of
      DocAppend a b -> DocAppend (rn a) (rn b)
      DocParagraph doc -> DocParagraph (rn doc)
      DocIdentifier x -> do
        let choices = dataTcOccs' x
        let names = concatMap (\c -> map gre_name (lookupGRE_RdrName c gre)) choices
        case names of
          [] ->
            case choices of
              [] -> DocMonospaced (DocString (showPpr dflags x))
              [a] -> outOfScope dflags a
              a:b:_ | isRdrTc a -> outOfScope dflags a
                    | otherwise -> outOfScope dflags b
          [a] -> DocIdentifier a
          a:b:_ | isTyConName a -> DocIdentifier a | otherwise -> DocIdentifier b
              -- If an id can refer to multiple things, we give precedence to type
              -- constructors.

      DocWarning doc -> DocWarning (rn doc)
      DocEmphasis doc -> DocEmphasis (rn doc)
      DocBold doc -> DocBold (rn doc)
      DocMonospaced doc -> DocMonospaced (rn doc)
      DocUnorderedList docs -> DocUnorderedList (map rn docs)
      DocOrderedList docs -> DocOrderedList (map rn docs)
      DocDefList list -> DocDefList [ (rn a, rn b) | (a, b) <- list ]
      DocCodeBlock doc -> DocCodeBlock (rn doc)
      DocIdentifierUnchecked x -> DocIdentifierUnchecked x
      DocModule str -> DocModule str
      DocHyperlink l -> DocHyperlink l
      DocPic str -> DocPic str
      DocAName str -> DocAName str
      DocProperty p -> DocProperty p
      DocExamples e -> DocExamples e
      DocEmpty -> DocEmpty
      DocString str -> DocString str
      DocHeader (Header l t) -> DocHeader $ Header l (rn t)

dataTcOccs' :: RdrName -> [RdrName]
-- If the input is a data constructor, return both it and a type
-- constructor.  This is useful when we aren't sure which we are
-- looking at.
--
-- We use this definition instead of the GHC's to provide proper linking to
-- functions accross modules. See ticket #253 on Haddock Trac.
dataTcOccs' rdr_name
  | isDataOcc occ             = [rdr_name, rdr_name_tc]
  | otherwise                 = [rdr_name]
  where
    occ = rdrNameOcc rdr_name
    rdr_name_tc = setRdrNameSpace rdr_name tcName


outOfScope :: DynFlags -> RdrName -> Doc a
outOfScope dflags x =
  case x of
    Unqual occ -> monospaced occ
    Qual mdl occ -> DocIdentifierUnchecked (mdl, occ)
    Orig _ occ -> monospaced occ
    Exact name -> monospaced name  -- Shouldn't happen since x is out of scope
  where
    monospaced a = DocMonospaced (DocString (showPpr dflags a))
