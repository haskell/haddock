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

import Control.Arrow ((***))
import Data.Bifunctor

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

instance Bifunctor DocH where
  second = fmap

  first _ DocEmpty = DocEmpty
  first f (DocAppend docA docB) = DocAppend (first f docA) (first f docB)
  first _ (DocString s) = DocString s
  first f (DocParagraph doc) = DocParagraph (first f doc)
  first _ (DocIdentifier identifier) = DocIdentifier identifier
  first f (DocIdentifierUnchecked m) = DocIdentifierUnchecked (f m)
  first _ (DocModule s) = DocModule s
  first f (DocWarning doc) = DocWarning (first f doc)
  first f (DocEmphasis doc) = DocEmphasis (first f doc)
  first f (DocMonospaced doc) = DocMonospaced (first f doc)
  first f (DocBold doc) = DocBold (first f doc)
  first f (DocUnorderedList docs) = DocUnorderedList (map (first f) docs)
  first f (DocOrderedList docs) = DocUnorderedList (map (first f) docs)
  first f (DocDefList docs) = DocDefList (map (first f *** first f) docs)
  first f (DocCodeBlock doc) = DocCodeBlock (first f doc)
  first _ (DocHyperlink h) = DocHyperlink h
  first _ (DocPic p) = DocPic p
  first _ (DocMathInline s) = DocMathInline s
  first _ (DocMathDisplay s) = DocMathDisplay s
  first _ (DocAName s) = DocAName s
  first _ (DocProperty s) = DocProperty s
  first _ (DocExamples examples) = DocExamples examples
  first f (DocHeader (Header l t)) = DocHeader (Header l (first f t))

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
