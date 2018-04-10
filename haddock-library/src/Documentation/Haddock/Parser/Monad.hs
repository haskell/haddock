{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Documentation.Haddock.Parser.Monad where

import qualified Text.Parsec.Char as Parsec
import qualified Text.Parsec as Parsec

import qualified Data.Text as T

import           Data.Text (Text)

import           Control.Applicative
import           Control.Monad
import           Data.String
import           Data.Bits
import           Data.Char (isDigit, ord, isHexDigit)
import           Data.List (foldl')

import           Documentation.Haddock.Types (Version)

newtype ParserState = ParserState {
  parserStateSince :: Maybe Version
} deriving (Eq, Show)

initialParserState :: ParserState
initialParserState = ParserState Nothing

type Parser = Parsec.Parsec Text ParserState

instance (a ~ Text) => IsString (Parser a) where
  fromString = fmap T.pack . Parsec.string

parseOnly :: Parser a -> Text -> Either String (ParserState, a)
parseOnly p t = case Parsec.runParser p' initialParserState "<haddock>" t of
                  Left e -> Left (show e)
                  Right (x,s) -> Right (s,x)
  where p' = (,) <$> p <*> Parsec.getState

setParserState :: ParserState -> Parser ()
setParserState = Parsec.putState

setSince :: Version -> Parser ()
setSince since = Parsec.modifyState (\st -> st {parserStateSince = Just since})

char :: Char -> Parser Char
char =  Parsec.char

many' :: Parser a -> Parser [a]
many' = Parsec.manyAccum (\x xs -> x `seq` x : xs)

anyChar :: Parser Char
anyChar = Parsec.anyChar

notChar :: Char -> Parser Char
notChar c = Parsec.satisfy (/= c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy = Parsec.satisfy

peekChar :: Parser (Maybe Char)
peekChar = Parsec.optionMaybe . Parsec.try . Parsec.lookAhead $ Parsec.anyChar

peekChar' :: Parser Char
peekChar' = Parsec.lookAhead $ Parsec.anyChar 

digit :: Parser Char
digit = Parsec.digit

space :: Parser Char
space = Parsec.space

string :: Text -> Parser Text
string = fmap T.pack . Parsec.string . T.unpack

skipSpace :: Parser ()
skipSpace = Parsec.skipMany Parsec.space

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile = Parsec.skipMany . Parsec.satisfy

take :: Int -> Parser Text
take = fmap T.pack . go
  where go !n | n <= 0 = pure []
              | otherwise = liftA2 (:) anyChar (go (n - 1)) <|> pure ""

scan :: s -> (s -> Char -> Maybe s) -> Parser Text 
scan s f = fmap T.pack (go s)
  where go s1 = do { cOpt <- peekChar
                   ; case cOpt >>= f s1 of
                       Nothing -> pure ""
                       Just s2 -> liftA2 (:) anyChar (go s2)
                   }

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile = fmap T.pack . Parsec.many . Parsec.satisfy

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 =  fmap T.pack . Parsec.many1 . Parsec.satisfy

endOfLine :: Parser ()
endOfLine = void Parsec.endOfLine

decimal :: Integral a => Parser a
decimal = foldl' step 0 `fmap` Parsec.many1 (satisfy isDigit)
  where step a c = a * 10 + fromIntegral (ord c - 48)

hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = foldl' step 0 `fmap` Parsec.many1 (satisfy isHexDigit)
  where
  step a c | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
           | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
           | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
    where w = ord c

endOfInput :: Parser ()
endOfInput = Parsec.eof
