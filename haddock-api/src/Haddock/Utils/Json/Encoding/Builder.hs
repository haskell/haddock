{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:      Data.Aeson.Encoding.Builder
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2013 Simon Meier <iridcode@gmail.com>
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value using the UTF-8 encoding.

module Haddock.Utils.Json.Encoding.Builder
    (
      encodeToBuilder
    , null_
    , bool
    , array
    , emptyArray_
    , emptyObject_
    , object
    , text
    , string
    , unquoted
    , quote
    , scientific
    , ascii2
    , ascii4
    , ascii5
    ) where

import Haddock.Utils.Json.Types (Value (..), KeyMap, Key)
import qualified Data.List as KM
import qualified Data.Foldable as KM
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim ((>$<), (>*<))
import qualified Data.ByteString.Builder.Prim as BP
import Data.Char (chr, ord)
import Data.Text.Encoding (encodeUtf8BuilderEscaped)
import Data.Word (Word8)
import qualified Data.Text as T

-- | Encode a JSON value to a "Data.ByteString" 'B.Builder'.
--
-- Use this function if you are encoding over the wire, or need to
-- prepend or append further bytes to the encoded JSON value.
encodeToBuilder :: Value -> Builder
encodeToBuilder Null       = null_
encodeToBuilder (Bool b)   = bool b
encodeToBuilder (Number n) = scientific n
encodeToBuilder (String s) = text s
encodeToBuilder (Array v)  = array v
encodeToBuilder (Object m) = object m

-- | Encode a JSON null.
null_ :: Builder
null_ = BP.primBounded (ascii4 ('n',('u',('l','l')))) ()

-- | Encode a JSON boolean.
bool :: Bool -> Builder
bool = BP.primBounded (BP.condB id (ascii4 ('t',('r',('u','e'))))
                                   (ascii5 ('f',('a',('l',('s','e'))))))

-- | Encode a JSON array.
array :: [] Value -> Builder
array v
  | null v  = emptyArray_
  | otherwise = B.char8 '[' <>
                encodeToBuilder (head v) <>
                foldr withComma (B.char8 ']') (tail v)
  where
    withComma a z = B.char8 ',' <> encodeToBuilder a <> z

-- Encode a JSON object.
object :: KeyMap Value -> Builder
object m = case KM.toList m of
    (x:xs) -> B.char8 '{' <> one x <> foldr withComma (B.char8 '}') xs
    _      -> emptyObject_
  where
    withComma a z = B.char8 ',' <> one a <> z
    one (k,v)     = key k <> B.char8 ':' <> encodeToBuilder v

-- | Encode a JSON key.
key :: Key -> Builder
key = text

-- | Encode a JSON string.
text :: T.Text -> Builder
text t = B.char8 '"' <> unquoted t <> B.char8 '"'

-- | Encode a JSON string, without enclosing quotes.
unquoted :: T.Text -> Builder
unquoted = encodeUtf8BuilderEscaped escapeAscii

-- | Add quotes surrounding a builder
quote :: Builder -> Builder
quote b = B.char8 '"' <> b <> B.char8 '"'

-- | Encode a JSON string.
string :: String -> Builder
string t = B.char8 '"' <> BP.primMapListBounded go t <> B.char8 '"'
  where go = BP.condB (> '\x7f') BP.charUtf8 (c2w >$< escapeAscii)

escapeAscii :: BP.BoundedPrim Word8
escapeAscii =
    BP.condB (== c2w '\\'  ) (ascii2 ('\\','\\')) $
    BP.condB (== c2w '\"'  ) (ascii2 ('\\','"' )) $
    BP.condB (>= c2w '\x20') (BP.liftFixedToBounded BP.word8) $
    BP.condB (== c2w '\n'  ) (ascii2 ('\\','n' )) $
    BP.condB (== c2w '\r'  ) (ascii2 ('\\','r' )) $
    BP.condB (== c2w '\t'  ) (ascii2 ('\\','t' )) $
    BP.liftFixedToBounded hexEscape -- fallback for chars < 0x20
  where
    hexEscape :: BP.FixedPrim Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) BP.>$<
        BP.char8 >*< BP.char8 >*< BP.word16HexFixed
{-# INLINE escapeAscii #-}

c2w :: Char -> Word8
c2w c = fromIntegral (ord c)

-- | Encode a JSON number.
scientific :: Double -> Builder
scientific = B.doubleDec

emptyArray_ :: Builder
emptyArray_ = BP.primBounded (ascii2 ('[',']')) ()

emptyObject_ :: Builder
emptyObject_ = BP.primBounded (ascii2 ('{','}')) ()

ascii2 :: (Char, Char) -> BP.BoundedPrim a
ascii2 cs = BP.liftFixedToBounded $ const cs BP.>$< BP.char7 >*< BP.char7
{-# INLINE ascii2 #-}

ascii3 :: (Char, (Char, Char)) -> BP.BoundedPrim a
ascii3 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii3 #-}

ascii4 :: (Char, (Char, (Char, Char))) -> BP.BoundedPrim a
ascii4 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii4 #-}

ascii5 :: (Char, (Char, (Char, (Char, Char)))) -> BP.BoundedPrim a
ascii5 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii5 #-}

ascii6 :: (Char, (Char, (Char, (Char, (Char, Char))))) -> BP.BoundedPrim a
ascii6 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii6 #-}

ascii8 :: (Char, (Char, (Char, (Char, (Char, (Char, (Char, Char)))))))
       -> BP.BoundedPrim a
ascii8 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii8 #-}

data T = T {-# UNPACK #-} !Char {-# UNPACK #-} !Char

twoDigits :: Int -> T
twoDigits a     = T (digit hi) (digit lo)
  where (hi,lo) = a `quotRem` 10

digit :: Int -> Char
digit x = chr (x + 48)

