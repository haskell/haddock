{-# LANGUAGE CPP #-}

module Documentation.Markdown.Util (
    joinLines
  , tabFilter
  , isWhitespace
  , isEscapable
  , normalizeReference
  , Scanner
  , scanIndentSpace
  , scanNonindentSpace
  , scanSpacesToColumn
  , scanChar
  , scanBlankline
  , scanSpaces
  , scanSpnl
  , nfb
  , nfbChar
  , upToCountChars
  , stripStart
  , stripEnd
  , strip
  , stripSuffix
  ) where

import Data.Char
#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Documentation.Markdown.ParserCombinators
import qualified Data.List as L

stripStart :: String -> String
stripStart = dropWhile isSpace

stripEnd :: String -> String
stripEnd = L.dropWhileEnd isSpace

strip :: String -> String
strip = stripEnd . stripStart 

stripSuffix :: String -> String -> Maybe String
stripSuffix p t
  | p `L.isSuffixOf` t = Just (take (length t - length p) p)
  | otherwise        = Nothing

-- Utility functions.

-- Like T.unlines but does not add a final newline.
-- Concatenates lines with newlines between.
joinLines :: [String] -> String
joinLines = L.intercalate "\n"

split :: (Char -> Bool) -> String -> [String]
split p t = loop t
    where loop s | null s'   = [l]
                 | otherwise = l : loop (tail s')
                    where (l, s') = span (not . p) s

justifyLeft :: Int -> Char -> String -> String
justifyLeft k c t
    | len >= k  = t
    | otherwise = t ++ replicate (k-len) c
    where len = length t

-- Convert tabs to spaces using a 4-space tab stop.
tabFilter :: String -> String
tabFilter = concat . pad . split (== '\t')
  where pad []  = []
        pad [t] = [t]
        pad (t:ts) = let tl = length t
                         n  = tl + 4 - (tl `mod` 4)
                         in  justifyLeft n ' ' t : pad ts

-- These are the whitespace characters that are significant in
-- parsing markdown. We can treat \160 (nonbreaking space) etc.
-- as regular characters.  This function should be considerably
-- faster than the unicode-aware isSpace from Data.Char.
isWhitespace :: Char -> Bool
isWhitespace ' '  = True
isWhitespace '\t' = True
isWhitespace '\n' = True
isWhitespace '\r' = True
isWhitespace _    = False

-- The original Markdown only allowed certain symbols
-- to be backslash-escaped.  It was hard to remember
-- which ones could be, so we now allow any ascii punctuation mark or
-- symbol to be escaped, whether or not it has a use in Markdown.
isEscapable :: Char -> Bool
isEscapable c = isAscii c && (isSymbol c || isPunctuation c)

-- Link references are case sensitive and ignore line breaks
-- and repeated spaces.
-- So, [APPLES are good] == [Apples are good] ==
-- [Apples
-- are     good].
normalizeReference :: String -> String
normalizeReference = {- T.toCaseFold . -} concat . split isWhitespace

-- Scanners are implemented here as attoparsec parsers,
-- which consume input and capture nothing.  They could easily
-- be implemented as regexes in other languages, or hand-coded.
-- With the exception of scanSpnl, they are all intended to
-- operate on a single line of input (so endOfInput = endOfLine).
type Scanner = Parser ()

-- Scan four spaces.
scanIndentSpace :: Scanner
scanIndentSpace = () <$ count 4 (skip (==' '))

scanSpacesToColumn :: Int -> Scanner
scanSpacesToColumn col = do
  currentCol <- column <$> getPosition
  case col - currentCol of
       n | n >= 1 -> () <$ (count n (skip (==' ')))
         | otherwise -> return ()

-- Scan 0-3 spaces.
scanNonindentSpace :: Scanner
scanNonindentSpace = () <$ upToCountChars 3 (==' ')

-- Scan a specified character.
scanChar :: Char -> Scanner
scanChar c = skip (== c) >> return ()

-- Scan a blankline.
scanBlankline :: Scanner
scanBlankline = scanSpaces *> endOfInput

-- Scan 0 or more spaces
scanSpaces :: Scanner
scanSpaces = skipWhile (==' ')

-- Scan 0 or more spaces, and optionally a newline
-- and more spaces.
scanSpnl :: Scanner
scanSpnl = scanSpaces *> option () (char '\n' *> scanSpaces)

-- Not followed by: Succeed without consuming input if the specified
-- scanner would not succeed.
nfb :: Parser a -> Scanner
nfb = notFollowedBy

-- Succeed if not followed by a character. Consumes no input.
nfbChar :: Char -> Scanner
nfbChar c = nfb (skip (==c))

upToCountChars :: Int -> (Char -> Bool) -> Parser String
upToCountChars cnt f =
  scan 0 (\n c -> if n < cnt && f c then Just (n+1) else Nothing)
