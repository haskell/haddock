{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LText

projectName :: Text
projectName = "Haddock"

projectUrl :: Text
projectUrl  = "http://www.haskell.org/haddock/"

projectVersion :: Text
projectVersion = LText.pack (showVersion version)
