{-# LANGUAGE CPP #-}


import Data.Char
import Data.Function (on)
import Data.Time.Clock

import System.Environment
import System.Exit
import System.FilePath

import Test.Haddock
import Test.Haddock.Xhtml

main :: IO ()
main = do
  cfg <- parseArgs checkConfig dirConfig =<< getArgs

  solo <- timeRun cfg 1
  mult <- timeRun cfg 8

  if mult < solo
  then do
    putStrLn "mult was faster than solo"
    exitSuccess
  else do
    putStrLn "mult was slow"
    exitFailure

timeRun :: Config c -> Int -> IO Double
timeRun cfg n = do
  start <- getCurrentTime
  runHaddock $ cfg
    { cfgHaddockArgs = cfgHaddockArgs cfg ++
      [ "--pretty-html"
      , "--html"
      , "--optghc=-j" ++ show n
      ]
    }
  end <- getCurrentTime
  return (realToFrac $ end `diffUTCTime` start)


checkConfig :: CheckConfig Xml
checkConfig = CheckConfig
    { ccfgRead = parseXml
    , ccfgClean = stripIfRequired
    , ccfgDump = dumpXml
    , ccfgEqual = (==) `on` dumpXml
    }


dirConfig :: DirConfig
dirConfig = (defaultDirConfig $ takeDirectory __FILE__)
    { dcfgCheckIgnore = checkIgnore
    }

stripIfRequired :: String -> Xml -> Xml
stripIfRequired mdl = stripLinks . stripFooter

checkIgnore :: FilePath -> Bool
checkIgnore file@(c:_) | takeExtension file == ".html" && isUpper c = False
checkIgnore _ = True
