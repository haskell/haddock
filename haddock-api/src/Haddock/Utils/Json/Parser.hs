{-# language PartialTypeSignatures #-}

-- | Json "Parsec" parser, based on
-- [json](https://hackage.haskell.org/package/json) package.
--
module Haddock.Utils.Json.Parser
  ( parseJSONValue
  ) where

import Prelude hiding (null)

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Parser

parseJSONValue :: _ Value
parseJSONValue = json
