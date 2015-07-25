module Haddock.Backends.Hyperlinker.Utils
    ( hypSrcDir, hypSrcModuleFile, hypSrcModuleFile'
    , hypSrcModuleUrl, hypSrcModuleUrl'
    , hypSrcNameUrl
    , hypSrcLineUrl
    , hypSrcModuleNameUrl, hypSrcModuleLineUrl
    , hypSrcModuleUrlFormat
    , hypSrcModuleNameUrlFormat, hypSrcModuleLineUrlFormat
    ) where


import Haddock.Backends.Xhtml.Utils

import GHC
import System.FilePath.Posix ((</>))


hypSrcDir :: FilePath
hypSrcDir = "src"

hypSrcModuleFile :: Module -> FilePath
hypSrcModuleFile = hypSrcModuleFile' . moduleName

hypSrcModuleFile' :: ModuleName -> FilePath
hypSrcModuleFile' mdl = spliceURL'
    Nothing (Just mdl) Nothing Nothing moduleFormat

hypSrcModuleUrl :: Module -> String
hypSrcModuleUrl = hypSrcModuleFile

hypSrcModuleUrl' :: ModuleName -> String
hypSrcModuleUrl' = hypSrcModuleFile'

hypSrcNameUrl :: Name -> String
hypSrcNameUrl name = spliceURL
    Nothing Nothing (Just name) Nothing nameFormat

hypSrcLineUrl :: SrcSpan -> String
hypSrcLineUrl spn = spliceURL
    Nothing Nothing Nothing (Just spn) lineFormat

hypSrcModuleNameUrl :: Module -> Name -> String
hypSrcModuleNameUrl mdl name = hypSrcModuleUrl mdl ++ "#" ++ hypSrcNameUrl name

hypSrcModuleLineUrl :: Module -> SrcSpan -> String
hypSrcModuleLineUrl mdl spn = hypSrcModuleUrl mdl ++ "#" ++ hypSrcLineUrl spn

hypSrcModuleUrlFormat :: String
hypSrcModuleUrlFormat = hypSrcDir </> moduleFormat

hypSrcModuleNameUrlFormat :: String
hypSrcModuleNameUrlFormat = hypSrcModuleUrlFormat ++ "#" ++ nameFormat

hypSrcModuleLineUrlFormat :: String
hypSrcModuleLineUrlFormat = hypSrcModuleUrlFormat ++ "#" ++ lineFormat

moduleFormat :: String
moduleFormat = "%{MODULE}.html"

nameFormat :: String
nameFormat = "%{NAME}"

lineFormat :: String
lineFormat = "%{LINE}"
