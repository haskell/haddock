--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Options (
  parseHaddockOpts,
  Flag(..),
  getUsage,
  ghcFlags,
  ifacePairs
) where


import Haddock.Utils
import Haddock.Exception
import System.Console.GetOpt 


getUsage :: IO String
getUsage = do
  prog <- getProgramName
  return $ usageInfo (usageHeader prog) (options False)
  where
    usageHeader :: String -> String
    usageHeader prog = "Usage: " ++ prog ++ " [OPTION...] file...\n"


parseHaddockOpts :: [String] -> IO ([Flag], [String])
parseHaddockOpts words =
  case getOpt Permute (options True) words of
    (flags, args, []) -> return (flags, args)
    (_, _, errors)    -> do 
      usage <- getUsage
      throwE (concat errors ++ usage)


ghcFlags :: [Flag] -> [String]
ghcFlags flags = [ option | Flag_OptGhc option <- flags ]


ifacePairs :: [Flag] -> [(FilePath, FilePath)]
ifacePairs flags = [ parseIfaceOption s | Flag_ReadInterface s <- flags ]


parseIfaceOption :: String -> (FilePath, FilePath)
parseIfaceOption s = 
  case break (==',') s of
	(fpath,',':file) -> (fpath, file)
	(file, _)        -> ("", file)


data Flag
  = Flag_CSS String
  | Flag_Debug
--  | Flag_DocBook
  | Flag_ReadInterface String
  | Flag_DumpInterface String
  | Flag_Heading String
  | Flag_Html
  | Flag_Hoogle
  | Flag_HtmlHelp String
  | Flag_Lib String
  | Flag_NoImplicitPrelude
  | Flag_OutputDir FilePath
  | Flag_Prologue FilePath
  | Flag_SourceBaseURL   String
  | Flag_SourceModuleURL String
  | Flag_SourceEntityURL String
  | Flag_WikiBaseURL   String
  | Flag_WikiModuleURL String
  | Flag_WikiEntityURL String
  | Flag_Help
  | Flag_Verbose
  | Flag_Version
  | Flag_UseContents String
  | Flag_GenContents
  | Flag_UseIndex String
  | Flag_GenIndex
  | Flag_IgnoreAllExports
  | Flag_HideModule String
  | Flag_OptGhc String
  | Flag_GhcLibDir String
  | Flag_GhcVersion
  deriving (Eq)


options :: Bool -> [OptDescr Flag]
options backwardsCompat =
  [
   Option ['B']  []     (ReqArg Flag_GhcLibDir "DIR")
	"path to the GHC lib dir, e.g /usr/lib/ghc",
   Option ['o']  ["odir"]     (ReqArg Flag_OutputDir "DIR")
	"directory in which to put the output files",
   Option ['l']  ["lib"]         (ReqArg Flag_Lib "DIR") 
	"location of Haddock's auxiliary files",
   Option ['i'] ["read-interface"] (ReqArg Flag_ReadInterface "FILE")
	"read an interface from FILE",
   Option ['D']  ["dump-interface"] (ReqArg Flag_DumpInterface "FILE") 
  "interface file name",
--    Option ['S']  ["docbook"]  (NoArg Flag_DocBook)
--	"output in DocBook XML",
    Option ['h']  ["html"]     (NoArg Flag_Html)
	"output in HTML",
    Option []  ["hoogle"]     (NoArg Flag_Hoogle)
    "output for Hoogle",
    Option []  ["html-help"]    (ReqArg Flag_HtmlHelp "format")
	"produce index and table of contents in\nmshelp, mshelp2 or devhelp format (with -h)",
    Option []  ["source-base"]   (ReqArg Flag_SourceBaseURL "URL") 
	"URL for a source code link on the contents\nand index pages",
    Option ['s'] (if backwardsCompat then ["source", "source-module"] else ["source-module"])
        (ReqArg Flag_SourceModuleURL "URL")
	"URL for a source code link for each module\n(using the %{FILE} or %{MODULE} vars)",
    Option []  ["source-entity"]  (ReqArg Flag_SourceEntityURL "URL") 
  "URL for a source code link for each entity\n(using the %{FILE}, %{MODULE}, %{NAME},\n%{KIND} or %{LINE} vars)",
    Option []  ["comments-base"]   (ReqArg Flag_WikiBaseURL "URL")
	"URL for a comments link on the contents\nand index pages",
    Option []  ["comments-module"]  (ReqArg Flag_WikiModuleURL "URL") 
	"URL for a comments link for each module\n(using the %{MODULE} var)",
    Option []  ["comments-entity"]  (ReqArg Flag_WikiEntityURL "URL") 
  "URL for a comments link for each entity\n(using the %{FILE}, %{MODULE}, %{NAME},\n%{KIND} or %{LINE} vars)",
    Option ['c']  ["css"]         (ReqArg Flag_CSS "FILE") 
	"the CSS file to use for HTML output",
    Option ['p']  ["prologue"] (ReqArg Flag_Prologue "FILE")
	"file containing prologue text",
    Option ['t']  ["title"]    (ReqArg Flag_Heading "TITLE")
	"page heading",
    Option ['n']  ["no-implicit-prelude"] (NoArg Flag_NoImplicitPrelude)
 	"do not assume Prelude is imported",
    Option ['d']  ["debug"]  (NoArg Flag_Debug)
	"extra debugging output",
    Option ['?']  ["help"]  (NoArg Flag_Help)
	"display this help and exit",
    Option ['V']  ["version"]  (NoArg Flag_Version)
	"output version information and exit",
    Option ['v']  ["verbose"]  (NoArg Flag_Verbose)
        "increase verbosity",
    Option [] ["use-contents"] (ReqArg Flag_UseContents "URL")
	"use a separately-generated HTML contents page",
    Option [] ["gen-contents"] (NoArg Flag_GenContents)
	"generate an HTML contents from specified\ninterfaces",
    Option [] ["use-index"] (ReqArg Flag_UseIndex "URL")
	"use a separately-generated HTML index",
    Option [] ["gen-index"] (NoArg Flag_GenIndex)
	"generate an HTML index from specified\ninterfaces",
    Option [] ["ignore-all-exports"] (NoArg Flag_IgnoreAllExports)
	"behave as if all modules have the\nignore-exports atribute",
    Option [] ["hide"] (ReqArg Flag_HideModule "MODULE")
	"behave as if MODULE has the hide attribute",
    Option [] ["optghc"] (ReqArg Flag_OptGhc "OPTION")
 	"Forward option to GHC",
    Option []  ["ghc-version"]  (NoArg Flag_GhcVersion)
	"output GHC version in numeric format"
   ]
