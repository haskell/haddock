{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception
import Data.Char (isSpace)
import Documentation.Haddock (haddock)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = getArgs >>= expandResponse >>= haddock


-- | Arguments which look like '@foo' will be replaced with the
-- contents of file @foo@. A gcc-like syntax for response files arguments
-- is expected.  This must re-constitute the argument list by doing an
-- inverse of the escaping mechanism done by the calling-program side.
--
-- We quit if the file is not found or reading somehow fails.
expandResponse :: [String] -> IO [String]
expandResponse = fmap concat . mapM expand
  where
    expand :: String -> IO [String]
    expand ('@':f) = readFileExc f >>= return . filter (not . null) . unescape
    expand x = return [x]

    readFileExc f =
      readFile f `catch` \(e :: IOException) -> do
        hPutStrLn stderr $ "Error while expanding response file: " ++ show e
        exitFailure

-- Support a gcc-like syntax for response files.
-- Single and double quotes, used as an escape, are mutually exclusive, but
-- either may have any number of back-slash escaped characters.  Back-
-- slash as an escape may also occur outside of any single/double quotes
-- used for escaping.  Any unescaped white-space denotes the end of a
-- given argument.  The single-quote, double-quote, and back-slash,
-- when used as escapes, are dropped from the re-constituted arguments.
-- Unescaped single- or double-quotes escape every character (but back-slash
-- is special) until the matching, unescaped quote; no matter where found,
-- a back-slash always escapes the immediately following character only.

data Quoting = NoneQ | SngQ | DblQ

unescape :: String -> [String]
unescape args = reverse . map reverse $ go args NoneQ False [] []
    where
      -- n.b., the order of these cases matters; these are cribbed from gcc
      -- case 1: end of input
      go []     _q    _bs   a as = a:as
      -- case 2: back-slash escape in progress
      go (c:cs) q     True  a as = go cs q     False (c:a) as
      -- case 3: no back-slash escape in progress, but got a back-slash
      go (c:cs) q     False a as
        | '\\' == c              = go cs q     True  a     as
      -- case 4: single-quote escaping in progress
      go (c:cs) SngQ  False a as
        | '\'' == c              = go cs NoneQ False a     as
        | otherwise              = go cs SngQ  False (c:a) as
      -- case 5: double-quote escaping in progress
      go (c:cs) DblQ  False a as
        | '"' == c               = go cs NoneQ False a     as
        | otherwise              = go cs DblQ  False (c:a) as
      -- case 6: no escaping is in progress
      go (c:cs) NoneQ False a as
        | isSpace c              = go cs NoneQ False []    (a:as)
        | '\'' == c              = go cs SngQ  False a     as
        | '"'  == c              = go cs DblQ  False a     as
        | otherwise              = go cs NoneQ False (c:a) as
