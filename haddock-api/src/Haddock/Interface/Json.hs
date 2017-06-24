{-# LANGUAGE RecordWildCards #-}
module Haddock.Interface.Json (
    jsonInstalledInterface
  , jsonInterfaceFile
  , renderJson
  ) where

import BasicTypes
import Json
import Module
import Name
import Outputable

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as Map

import Haddock.Types
import Haddock.InterfaceFile

jsonInterfaceFile :: InterfaceFile -> JsonDoc
jsonInterfaceFile InterfaceFile{..} =
  jsonObject [ ("link_env" , jsonNull)
             , ("inst_ifaces", jsonArray (map jsonInstalledInterface ifInstalledIfaces))
             ]

jsonInstalledInterface :: InstalledInterface -> JsonDoc
jsonInstalledInterface InstalledInterface{..} = jsonObject properties
  where
    properties =
      [ ("module"          , jsonModule instMod)
      , ("is_sig"          , jsonBool instIsSig)
      , ("info"            , jsonHaddockModInfo instInfo)
      , ("doc_map"         , jsonMap nameStableString jsonMDoc instDocMap)
      , ("arg_map"         , jsonMap nameStableString (jsonMap show jsonMDoc) instArgMap)
      , ("exports"         , jsonArray (map jsonName instExports))
      , ("visible_exports" , jsonArray (map jsonName instVisibleExports))
      , ("options"         , jsonArray (map (jsonString . show) instOptions))
      , ("sub_map"         , jsonMap nameStableString (jsonArray . map jsonName) instSubMap)
      , ("bundled_patsyns" , jsonMap nameStableString (jsonArray . map jsonName) instBundledPatSynMap)
      , ("fix_map"         , jsonMap nameStableString jsonFixity instFixMap)
      ]

    jsonMap :: (a -> String) -> (b -> JsonDoc) -> Map a b -> JsonDoc
    jsonMap f g = jsonObject . map (f *** g) .  Map.toList

jsonHaddockModInfo :: HaddockModInfo Name -> JsonDoc
jsonHaddockModInfo HaddockModInfo{..} =
  jsonObject [ ("description" , jsonMaybe jsonDoc hmi_description)
             , ("copyright"   , jsonMaybe jsonString hmi_copyright)
             , ("maintainer"  , jsonMaybe jsonString hmi_maintainer)
             , ("stability"   , jsonMaybe jsonString hmi_stability)
             , ("protability" , jsonMaybe jsonString hmi_portability)
             , ("safety"      , jsonMaybe jsonString hmi_safety)
             , ("language"    , jsonMaybe (const jsonNull) hmi_language)
             , ("extensions"  , jsonArray (map (const jsonNull) hmi_extensions))
             ]

jsonMDoc :: MDoc Name -> JsonDoc
jsonMDoc _ = jsonNull

jsonDoc :: Doc Name -> JsonDoc
jsonDoc _ = jsonNull

jsonModule :: Module -> JsonDoc
jsonModule = JSString . moduleStableString

jsonName :: Name -> JsonDoc
jsonName = JSString . nameStableString

jsonFixity :: Fixity -> JsonDoc
jsonFixity (Fixity _ prec dir) =
  jsonObject [ ("prec"      , jsonInt prec)
             , ("direction" , jsonFixityDirection dir)
             ]

jsonFixityDirection :: FixityDirection -> JsonDoc
jsonFixityDirection InfixL = jsonString "infixl"
jsonFixityDirection InfixR = jsonString "infixr"
jsonFixityDirection InfixN = jsonString "infix"

renderJson :: JsonDoc -> SDoc
renderJson = renderJSON

jsonMaybe :: (a -> JsonDoc) -> Maybe a -> JsonDoc
jsonMaybe f = maybe jsonNull f 

jsonString :: String -> JsonDoc
jsonString = JSString

jsonObject :: [(String, JsonDoc)] -> JsonDoc
jsonObject = JSObject

jsonArray :: [JsonDoc] -> JsonDoc
jsonArray = JSArray

jsonNull :: JsonDoc
jsonNull = JSNull

jsonInt :: Int -> JsonDoc
jsonInt = JSInt

jsonBool :: Bool -> JsonDoc
jsonBool = JSBool

