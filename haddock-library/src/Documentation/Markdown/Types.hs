{-# LANGUAGE OverloadedStrings #-}
module Documentation.Markdown.Types where

import Documentation.Haddock.Types
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Data.Semigroup

data ListType = Ordered | Unordered
  deriving (Show, Eq)

data Block inline
    = BlockPara inline
    | BlockList ListType (Either inline [Block inline])
    | BlockCode (Maybe String) String
    | BlockQuote [Block inline]
    | BlockRule
    | BlockHeading Int inline
    | BlockReference String String
    | BlockPlainText inline
  deriving (Show, Eq)

instance Functor Block where
    fmap f (BlockPara i) = BlockPara (f i)
    fmap f (BlockList lt (Left i)) = BlockList lt $ Left $ f i
    fmap f (BlockList lt (Right bs)) = BlockList lt $ Right $ map (fmap f) bs
    fmap _ (BlockCode a b) = BlockCode a b
    fmap f (BlockQuote bs) = BlockQuote $ map (fmap f) bs
    fmap _ BlockRule = BlockRule
    fmap f (BlockHeading level i) = BlockHeading level (f i)
    fmap _ (BlockReference x y) = BlockReference x y
    fmap f (BlockPlainText x) = BlockPlainText (f x)

data Inline = InlineText String
            | InlineItalic [Inline]
            | InlineBold [Inline]
            | InlineCode String
            | InlineLink String (Maybe String) [Inline] -- ^ URL, title, content
            | InlineImage String (Maybe String) String -- ^ URL, title, content
            | InlineFootnoteRef Integer -- ^ The footnote reference in the body
            | InlineFootnote Integer
    deriving (Show, Eq)

inlineToDoc :: Inline -> DocH a b 
inlineToDoc (InlineText str) = DocString str
inlineToDoc (InlineItalic is) = DocEmphasis (foldMap inlineToDoc is)
inlineToDoc (InlineBold is) = DocBold (foldMap inlineToDoc is)
inlineToDoc (InlineCode str) = DocMonospaced (DocString str)
inlineToDoc (InlineLink url title _) = DocHyperlink (Hyperlink url title)
inlineToDoc (InlineImage url title _) = DocPic (Picture url title)
inlineToDoc _ = error "unimplemented"

blockToDoc :: Block (DocH a b) -> DocH a b
blockToDoc (BlockPara d) = DocParagraph d
blockToDoc (BlockCode _ c) = DocCodeBlock (DocString c)
blockToDoc (BlockHeading lvl d) = DocHeader (Header lvl d)
blockToDoc (BlockPlainText d) = d
blockToDoc _ = error "unimplemented"


instance Semigroup (DocH a b) where
  (<>) = DocAppend

instance Monoid (DocH a b) where
  mempty = DocEmpty
  mappend = (<>)

stripEnd :: String -> String
stripEnd = dropWhileEnd isSpace

stripStart :: String -> String
stripStart = dropWhile isSpace

strip :: String -> String
strip = stripEnd . stripStart
