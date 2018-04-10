{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Documentation.Haddock.Parser.Util
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Various utility functions used by the parser.
module Documentation.Haddock.Parser.Util (
  unsnoc
, strip
, inClass
, notInClass
, takeUntil
, removeEscapes
, makeLabeled
, takeHorizontalSpace
, skipHorizontalSpace
) where

import qualified Data.Text as T
import           Data.Text (Text)

import           Control.Applicative
import           Control.Monad (mfilter)
import           Documentation.Haddock.Parser.Monad
import           Prelude hiding (takeWhile)

import           Data.Maybe (isJust)
import           Data.Char (isSpace)

-- | Like 'T.uncons', but for the last character instead of the first.
unsnoc :: Text -> Maybe (Text, Char)
unsnoc bs
  | T.null bs = Nothing
  | otherwise = Just (T.init bs, T.last bs)

-- | Remove all leading and trailing whitespace
strip :: Text -> Text  
strip = T.strip 

-- | Like 'elem' but for 'Text'
inClass :: Char -> Text -> Bool
inClass c = isJust . T.find (== c)

-- | Like 'notElem' but for 'Text'
notInClass :: Char -> Text -> Bool
notInClass c = not . inClass c 

-- | Characters that count as horizontal space
horizontalSpace :: Text
horizontalSpace = " \t\f\v\r"

-- | Skip and ignore leading horizontal space
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile (`inClass` horizontalSpace)

-- | Take leading horizontal space
takeHorizontalSpace :: Parser Text 
takeHorizontalSpace = takeWhile (`inClass` horizontalSpace)

makeLabeled :: (String -> Maybe String -> a) -> Text -> a
makeLabeled f input = case T.break isSpace $ removeEscapes $ strip input of
  (uri, "")    -> f (T.unpack uri) Nothing
  (uri, label) -> f (T.unpack uri) (Just . T.unpack $ T.stripStart label)

-- | Remove escapes from given string.
--
-- Only do this if you do not process (read: parse) the input any further.
removeEscapes :: Text -> Text
removeEscapes = T.filter (/= '\\') . T.replace "\\\\" "\\"

-- | Consume characters from the input up to and including the given pattern.
-- Return everything consumed except for the end pattern itself.
takeUntil :: Text -> Parser Text 
takeUntil end_ = T.dropEnd (T.length end_) <$> requireEnd (scan (False, end) p) >>= gotSome
  where
    end = T.unpack end_ 

    p :: (Bool, String) -> Char -> Maybe (Bool, String)
    p acc c = case acc of
      (True, _) -> Just (False, end)
      (_, []) -> Nothing
      (_, x:xs) | x == c -> Just (False, xs)
      _ -> Just (c == '\\', end)

    requireEnd = mfilter (T.isSuffixOf end_)

    gotSome xs
      | T.null xs = fail "didn't get any content"
      | otherwise = return xs
