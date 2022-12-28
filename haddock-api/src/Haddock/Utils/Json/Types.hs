module Haddock.Utils.Json.Types
  ( Value(..)
  , typeOf
  , Object
  , object
  ) where

import Data.Aeson

-- TODO: We may want to replace 'String' with 'Text' or 'ByteString'

typeOf :: Value -> String
typeOf v = case v of
    Object _ -> "Object"
    Array _  -> "Array"
    String _ -> "String"
    Number _ -> "Number"
    Bool _   -> "Boolean"
    Null     -> "Null"
