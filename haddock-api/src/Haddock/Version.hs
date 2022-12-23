{-# LANGUAGE CPP, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Version
-- Copyright   :  (c) Simon Marlow 2003
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Version (
  projectName, projectVersion, projectUrl
) where

#ifdef IN_GHC_TREE
import Paths_haddock ( version )
#else
import Paths_haddock_api ( version )
#endif
import Data.Version  ( showVersion )
import Data.String (IsString(..))

projectName :: IsString s => s
projectName = "Haddock"

projectUrl :: IsString s => s
projectUrl  = "http://www.haskell.org/haddock/"

projectVersion :: String
projectVersion = showVersion version
