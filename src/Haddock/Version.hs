--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module Haddock.Version ( 
  projectName, projectVersion, projectUrl
) where

import Paths_haddock ( version )
import Data.Version  ( showVersion )

projectName, projectUrl :: String
projectName = "Haddock"
projectUrl  = "http://www.haskell.org/haddock/"

projectVersion :: String
projectVersion = showVersion version
