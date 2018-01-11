module Documentation.Markdown.Types where

import Data.Sequence (Seq)
import qualified Data.Map as M
import Data.Foldable (toList)

import qualified Documentation.Haddock.Types as H
import Documentation.Haddock.Doc (docConcat)

-- | Structured representation of a document.
data Doc = Doc Blocks
           deriving (Show)

-- | Block-level elements.
data Block = Para Inlines
           | Header Int Inlines
           | Blockquote Blocks
           | List Bool ListType [Blocks]
           | CodeBlock CodeAttr String
           | HtmlBlock String
           | Definition Inlines [Blocks]
           | HRule
           deriving (Show)

-- | Attributes for fenced code blocks.  'codeLang' is the
-- first word of the attribute line, 'codeInfo' is the rest.
data CodeAttr = CodeAttr { codeLang :: String, codeInfo :: String }
              deriving (Show)

data ListType = Bullet Char | Numbered NumWrapper Int deriving (Eq,Show)

data NumWrapper = PeriodFollowing | ParenFollowing deriving (Eq,Show)

-- | Simple representation of HTML tag.
data HtmlTagType = Opening String | Closing String | SelfClosing String deriving (Show)

-- We operate with sequences instead of lists, because
-- they allow more efficient appending on to the end.
type Blocks = Seq Block

-- | Inline elements.
data Inline = Str String
            | Space
            | LineBreak
            | Emph Inlines
            | Strong Inlines
            | Code String
            | Link Inlines String {- URL -} String {- title -}
            | Image Inlines String {- URL -} String {- title -}
            | Entity String
            | RawHtml String
            | Math String
            deriving (Show)

type Inlines = Seq Inline

type ReferenceMap = M.Map String (String, String)

docToDoc :: Doc -> H.DocH a b
docToDoc (Doc bs) = docConcat (map blockToDoc (toList bs))

inlineToDoc :: Inline -> H.DocH a b
inlineToDoc (Str str) = H.DocString str
inlineToDoc Space = H.DocString " "
inlineToDoc (Emph is) = H.DocEmphasis (docConcat (map inlineToDoc (toList is)))
inlineToDoc (Strong is) = H.DocBold (docConcat (map inlineToDoc (toList is)))
inlineToDoc (Code str) = H.DocMonospaced (H.DocString str)
inlineToDoc (Link is url _) = H.DocHyperlink (H.Hyperlink url (Just (docConcat (map inlineToDoc (toList is)))))
inlineToDoc (Image _ url title) = H.DocPic (H.Picture url (Just title))
inlineToDoc (Math str) = H.DocMathInline str
inlineToDoc _ = error "unimplemented"

blockToDoc :: Block -> H.DocH a b
blockToDoc (Para is) = H.DocParagraph (docConcat (map inlineToDoc (toList is)))
blockToDoc (Header lvl is) = H.DocHeader (H.Header lvl (docConcat (map inlineToDoc (toList is))))
blockToDoc (List _ (Bullet _) bs) = H.DocUnorderedList (map (docConcat . map blockToDoc . toList) bs)  -- TODO tight?
blockToDoc (List _ (Numbered _ _) bs) = H.DocOrderedList (map (docConcat . map blockToDoc . toList) bs) -- ^ ditto
blockToDoc (CodeBlock (CodeAttr lang _) str) = H.DocCodeBlock (H.CodeBlock (Just lang) (H.DocString str))
blockToDoc (Blockquote bs) = H.DocBlockQuote (docConcat (map blockToDoc (toList bs)))
blockToDoc (Definition is bs) = H.DocDefList [(docConcat (map inlineToDoc is (toList is)), docConcat (map blockToDoc (toList bs)))]
blockToDoc _ = error "unimplemented"

