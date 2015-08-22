{-# LANGUAGE CPP #-}


import System.Environment
import System.FilePath

import Test.Haddock


checkConfig :: CheckConfig String
checkConfig = CheckConfig
    { ccfgRead = \_ input -> Just input
    , ccfgDump = id
    , ccfgEqual = (==)
    }


dirConfig :: DirConfig
dirConfig = defaultDirConfig $ takeDirectory __FILE__


main :: IO ()
main = do
    cfg <- parseArgs checkConfig dirConfig =<< getArgs
    runAndCheck $ cfg
        { cfgHaddockArgs = cfgHaddockArgs cfg ++
            [ "--package-name=test"
            , "--package-version=0.0.0"
            , "--hoogle"
            ]
        }
