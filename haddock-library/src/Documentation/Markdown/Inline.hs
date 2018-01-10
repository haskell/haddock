{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module Documentation.Markdown.Inline
    ( Inline (..)
    , inlineParser
    , toInline
    ) where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Monoid (Monoid, mappend)
import qualified Data.Map as Map

import Data.Attoparsec.ByteString.Char8 as BA
import qualified Data.ByteString.Char8 as BS

import Documentation.Markdown.Types (Inline(..), stripEnd)

type RefMap = Map.Map String BS.ByteString

toInline :: RefMap -> BS.ByteString -> [Inline]
toInline refmap t =
    case parseOnly (inlineParser refmap) t of
        Left s -> [InlineText s]
        Right is -> is

(<>) :: Monoid m => m -> m -> m
(<>) = mappend


inlineParser :: RefMap -> Parser [Inline]
inlineParser = fmap combine . many . inlineAny

combine :: [Inline] -> [Inline]
combine [] = []
combine (InlineText x:InlineText y:rest) = combine (InlineText (x <> y):rest)
combine (InlineText x:rest) = InlineText x : combine rest
combine (InlineItalic x:InlineItalic y:rest) = combine (InlineItalic (x <> y):rest)
combine (InlineItalic x:rest) = InlineItalic (combine x) : combine rest
combine (InlineBold x:InlineBold y:rest) = combine (InlineBold (x <> y):rest)
combine (InlineBold x:rest) = InlineBold (combine x) : combine rest
combine (InlineCode x:InlineCode y:rest) = combine (InlineCode (x <> y):rest)
combine (InlineCode x:rest) = InlineCode x : combine rest
combine (InlineLink u t c:rest) = InlineLink u t (combine c) : combine rest
combine (InlineImage u t c:rest) = InlineImage u t c : combine rest
combine (InlineFootnote x:rest) = InlineFootnote x : combine rest
combine (InlineFootnoteRef x:rest) = InlineFootnoteRef x : combine rest

specials :: [Char]
specials = "*_`\\[]!<&{}"

inlineAny :: RefMap -> Parser Inline
inlineAny refs =
    inline refs <|> special
  where
    special = InlineText . pure <$> satisfy (`elem` specials)

inline :: RefMap -> Parser Inline
inline refs =
    text
    <|> escape
    <|> footnote
    <|> footnoteRef
    <|> paired "**" InlineBold <|> paired "__" InlineBold
    <|> paired "*" InlineItalic <|> paired "_" InlineItalic
    <|> doubleCodeSpace <|> doubleCode <|> code
    <|> link
    <|> image
    <|> autoLink
  where
    inlinesTill :: BS.ByteString -> Parser [Inline]
    inlinesTill end = go id
      where
        go front =
            (string end *> pure (front []))
            <|> (do
                x <- inlineAny refs
                go $ front . (x:))

    text = InlineText . BS.unpack <$> takeWhile1 (`notElem` specials)

    paired t wrap = wrap <$> do
        _ <- string t
        is <- inlinesTill t
        if null is then fail "wrapped around something missing" else return is

    doubleCodeSpace = InlineCode <$> (string "`` " *> manyTill anyChar (string " ``"))
    doubleCode = InlineCode <$> (string "``" *> manyTill anyChar (string "``"))
    code = InlineCode . BS.unpack  <$> (char '`' *> takeWhile1 (/= '`') <* char '`')
    
    dollarMath = InlineMath True <$> (char '$' *> (\c s -> satsify (not . isSpace) <*> manyTill

    footnoteRef = InlineFootnoteRef <$> (char '{' *> decimal <* char '}')
    footnote = InlineFootnote <$> (string "{^" *> decimal <* char '}')

    escape = InlineText . pure <$>
        (char '\\' *> satisfy (`elem` ("\\`*_{}[]()#+-.!>" :: String)))

    takeBalancedBrackets = go (0 :: Int)
      where
        go i = do
            c <- anyChar
            case c of
                '[' -> (c:) <$> go (i + 1)
                ']'
                    | i == 0 -> return []
                    | otherwise -> (c:) <$> go (i - 1)
                _ -> (c:) <$> go i

    parseUrl = fixUrl <$> parseUrl' (0 :: Int)

    parseUrl' level
        | level > 0 = do
            c <- anyChar
            let level'
                    | c == ')' = level - 1
                    | otherwise = level
            c' <-
                if c == '\\'
                    then anyChar
                    else return c
            cs <- parseUrl' level'
            return $ c' : cs
        | otherwise = (do
            c <- hrefChar
            if c == '('
                then (c:) <$> parseUrl' 1
                else (c:) <$> parseUrl' 0) <|> return []

    parseUrlTitle defRef = parseUrlTitleInline <|> parseUrlTitleRef defRef

    parseUrlTitleInside endTitle = do
        url <- parseUrl
        mtitle <- (Just <$> title) <|> (skipSpace >> endTitle >> pure Nothing)
        return (url, mtitle)
      where
        title = do
            _ <- space
            skipSpace
            _ <- char '"'
            t <- stripEnd <$> go
            return $
                if not (null t) && last t == '"'
                    then init t
                    else t
          where
            go =  (char '\\' *> anyChar >>= \c -> (c:) <$> go)
              <|> (endTitle *> return [])
              <|> (anyChar >>= \c -> (c:) <$> go)

    parseUrlTitleInline = char '(' *> parseUrlTitleInside (char ')')

    parseUrlTitleRef defRef = do
        ref' <- BS.unpack <$> ((skipSpace *> char '[' *> takeWhile (/= ']') <* char ']') <|> return "")
        let ref = if null ref' then defRef else ref'
        case Map.lookup (unwords $ words ref) refs of
            Nothing -> fail "ref not found"
            Just t -> either fail return $ parseOnly (parseUrlTitleInside endOfInput) t

    link = do
        _ <- char '['
        rawContent <- takeBalancedBrackets
        content <- either fail return $ parseOnly (inlineParser refs) (BS.pack rawContent)
        (url, mtitle) <- parseUrlTitle rawContent
        return $ InlineLink url mtitle content

    image = do
        _ <- string "!["
        content <- takeBalancedBrackets
        (url, mtitle) <- parseUrlTitle content
        return $ InlineImage url mtitle content

    fixUrl t
        | length t > 2 && head t == '<' && last t == '>' = init $ tail t
        | otherwise = t

    autoLink = do
        _ <- char '<'
        a <- string "http:" <|> string "https:"
        b <- takeWhile1 (/= '>')
        _ <- char '>'
        let url = BS.unpack a ++ BS.unpack b
        return $ InlineLink url Nothing [InlineText url]

    
hrefChar :: Parser Char
hrefChar = (char '\\' *> anyChar) <|> satisfy (notInClass " )")
