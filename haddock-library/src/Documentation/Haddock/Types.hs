{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- |
-- Module      :  Documentation.Haddock.Types
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskellorg
-- Stability   :  experimental
-- Portability :  portable
--
-- Exposes documentation data types used for (some) of Haddock.
module Documentation.Haddock.Types where

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable
import Data.Traversable
#endif

#if MIN_VERSION_base(4,8,0)
import Control.Arrow ((***))
import Data.Bifunctor
#endif

-- | With the advent of 'Version', we may want to start attaching more
-- meta-data to comments. We make a structure for this ahead of time
-- so we don't have to gut half the core each time we want to add such
-- info.
newtype Meta = Meta { _version :: Maybe Version } deriving (Eq, Show)

data MetaDoc mod id =
  MetaDoc { _meta :: Meta
          , _doc :: DocH mod id
          } deriving (Eq, Show, Functor, Foldable, Traversable)

overDoc :: (DocH a b -> DocH c d) -> MetaDoc a b -> MetaDoc c d
overDoc f d = d { _doc = f $ _doc d }

type Version = [Int]

data Hyperlink = Hyperlink
  { hyperlinkUrl   :: String
  , hyperlinkLabel :: Maybe String
  } deriving (Eq, Show)

data Picture = Picture
  { pictureUri   :: String
  , pictureTitle :: Maybe String
  } deriving (Eq, Show)

data Header id = Header
  { headerLevel :: Int
  , headerTitle :: id
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Example = Example
  { exampleExpression :: String
  , exampleResult     :: [String]
  } deriving (Eq, Show)

data DocH mod id
  = DocEmpty
  | DocAppend (DocH mod id) (DocH mod id)
  | DocString String
  | DocParagraph (DocH mod id)
  | DocIdentifier id
  | DocIdentifierUnchecked mod
  | DocModule String
  | DocWarning (DocH mod id)
  | DocEmphasis (DocH mod id)
  | DocMonospaced (DocH mod id)
  | DocBold (DocH mod id)
  | DocUnorderedList [DocH mod id]
  | DocOrderedList [DocH mod id]
  | DocDefList [(DocH mod id, DocH mod id)]
  | DocCodeBlock (DocH mod id)
  | DocHyperlink Hyperlink
  | DocPic Picture
  | DocMathInline String
  | DocMathDisplay String
  | DocAName String
  | DocProperty String
  | DocExamples [Example]
  | DocHeader (Header (DocH mod id))
  deriving (Eq, Show, Functor, Foldable, Traversable)

#if MIN_VERSION_base(4,8,0)
instance Bifunctor DocH where
  bimap _ _ DocEmpty = DocEmpty
  bimap f g (DocAppend docA docB) = DocAppend (bimap f g docA) (bimap f g docB)
  bimap _ _ (DocString s) = DocString s
  bimap f g (DocParagraph doc) = DocParagraph (bimap f g doc)
  bimap _ g (DocIdentifier i) = DocIdentifier (g i)
  bimap f _ (DocIdentifierUnchecked m) = DocIdentifierUnchecked (f m)
  bimap _ _ (DocModule s) = DocModule s
  bimap f g (DocWarning doc) = DocWarning (bimap f g doc)
  bimap f g (DocEmphasis doc) = DocEmphasis (bimap f g doc)
  bimap f g (DocMonospaced doc) = DocMonospaced (bimap f g doc)
  bimap f g (DocBold doc) = DocBold (bimap f g doc)
  bimap f g (DocUnorderedList docs) = DocUnorderedList (map (bimap f g) docs)
  bimap f g (DocOrderedList docs) = DocOrderedList (map (bimap f g) docs)
  bimap f g (DocDefList docs) = DocDefList (map (bimap f g *** bimap f g) docs)
  bimap f g (DocCodeBlock doc) = DocCodeBlock (bimap f g doc)
  bimap _ _ (DocHyperlink hyperlink) = DocHyperlink hyperlink
  bimap _ _ (DocPic picture) = DocPic picture
  bimap _ _ (DocMathInline s) = DocMathInline s
  bimap _ _ (DocMathDisplay s) = DocMathDisplay s
  bimap _ _ (DocAName s) = DocAName s
  bimap _ _ (DocProperty s) = DocProperty s
  bimap _ _ (DocExamples examples) = DocExamples examples
  bimap f g (DocHeader (Header level title)) = DocHeader (Header level (bimap f g title))
#endif

-- | 'DocMarkupH' is a set of instructions for marking up documentation.
-- In fact, it's really just a mapping from 'Doc' to some other
-- type [a], where [a] is usually the type of the output (HTML, say).
-- Use 'Documentation.Haddock.Markup.markup' to apply a 'DocMarkupH' to
-- a 'DocH'.
--
-- @since 1.4.5
--
data DocMarkupH mod id a = Markup
  { markupEmpty                :: a
  , markupString               :: String -> a
  , markupParagraph            :: a -> a
  , markupAppend               :: a -> a -> a
  , markupIdentifier           :: id -> a
  , markupIdentifierUnchecked  :: mod -> a
  , markupModule               :: String -> a
  , markupWarning              :: a -> a
  , markupEmphasis             :: a -> a
  , markupBold                 :: a -> a
  , markupMonospaced           :: a -> a
  , markupUnorderedList        :: [a] -> a
  , markupOrderedList          :: [a] -> a
  , markupDefList              :: [(a,a)] -> a
  , markupCodeBlock            :: a -> a
  , markupHyperlink            :: Hyperlink -> a
  , markupAName                :: String -> a
  , markupPic                  :: Picture -> a
  , markupMathInline           :: String -> a
  , markupMathDisplay          :: String -> a
  , markupProperty             :: String -> a
  , markupExample              :: [Example] -> a
  , markupHeader               :: Header a -> a
  }
