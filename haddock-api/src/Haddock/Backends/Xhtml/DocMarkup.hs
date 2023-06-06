{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.DocMarkup
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.DocMarkup (
  docToHtml,
  rdrDocToHtml,
  origDocToHtml,
  docToHtmlNoAnchors,

  docElement, docSection, docSection_,
) where

import Data.List (intersperse)
import Documentation.Haddock.Markup
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Utils
import Haddock.Types
import Haddock.Utils
import Haddock.Doc (combineDocumentation, emptyMetaDoc,
                    metaDocAppend, metaConcat)

import Data.Text(Text)
import qualified Data.Text as Text
import Lucid
import Data.Maybe (fromMaybe)

import GHC hiding (anchor)
import GHC.Types.Name


parHtmlMarkup :: Qualification -> Bool
              -> (Bool -> a -> Html ()) -> DocMarkup a (Html ())
parHtmlMarkup qual insertAnchors ppId = Markup {
  markupEmpty                = pure (),
  markupString               = toHtml,
  markupParagraph            = p_,
  markupAppend               = (<>),
  markupIdentifier           = code_ . ppId insertAnchors,
  markupIdentifierUnchecked  = code_ . ppUncheckedLink qual,
  markupModule               = \(ModLink m lbl) ->
                                 let (mdl,ref) = break (=='#') m
                                       -- Accommodate for old style
                                       -- foo\#bar anchors
                                     mdl' = case reverse mdl of
                                              '\\':_ -> init mdl
                                              _ -> mdl
                                 in ppModuleRef lbl (mkModuleName mdl') ref,
  markupWarning              = div_ [class_ "warning"],
  markupEmphasis             = em_,
  markupBold                 = strong_,
  markupMonospaced           = code_,
  markupUnorderedList        = ul_ . foldMap li_,
  markupOrderedList          = makeOrdList,
  markupDefList              = dl_ . foldMap (\(dt, dd) -> dt_ dt <> dd_ dd),
  markupCodeBlock            = pre_,
  markupHyperlink            = \(Hyperlink url mLabel)
                               -> if insertAnchors
                                  then a_ [href_ (Text.pack url)] (fromMaybe (toHtml (Text.pack url)) mLabel)
                                  else fromMaybe (toHtml (Text.pack url)) mLabel,
  markupAName                = \aname
                               -> if insertAnchors
                                  then namedAnchor (Text.pack aname) [] ""
                                  else pure (),
  markupPic                  = \(Picture uri t) -> img_ ([src_ (Text.pack uri)] ++ fromMaybe [] (return . title_ . Text.pack <$> t)),
  markupMathInline           = \mathjax -> span_ [class_ "mathjax"] (toHtml (Text.pack $ "\\(" ++ mathjax ++ "\\)")),
  markupMathDisplay          = \mathjax -> span_ [class_ "mathjax"] (toHtml (Text.pack $ "\\[" ++ mathjax ++ "\\]")),
  markupProperty             = pre_ . toHtml,
  markupExample              = examplesToHtml,
  markupHeader               = \(Header l t) -> makeHeader l t,
  markupTable                = \(Table h r) -> makeTable h r
  }
  where
    makeHeader :: Int -> Html () -> Html ()
    makeHeader 1 mkup = h1_ mkup
    makeHeader 2 mkup = h2_ mkup
    makeHeader 3 mkup = h3_ mkup
    makeHeader 4 mkup = h4_ mkup
    makeHeader 5 mkup = h5_ mkup
    makeHeader 6 mkup = h6_ mkup
    makeHeader l _ = error $ "Somehow got a header level `" ++ show l ++ "' in DocMarkup!"

    makeTable :: [TableRow (Html ())] -> [TableRow (Html ())] -> Html ()
    makeTable hs bs = table_ (mconcat (hs' ++ bs'))
      where
        hs' | null hs   = []
            | otherwise = [thead_ (mconcat (map (makeTableRow th_) hs))]

        bs' = [tbody_ (mconcat (map (makeTableRow td_) bs))]

    makeTableRow :: ([Attributes] -> Html () -> Html ()) -> TableRow (Html ()) -> Html ()
    makeTableRow thr (TableRow cs) = tr_ (mconcat (map (makeTableCell thr) cs))

    makeTableCell :: ([Attributes] -> Html () -> Html ()) -> TableCell (Html ()) -> Html ()
    makeTableCell thr (TableCell i j c) = thr (i' ++ j') c
      where
        i' = if i == 1 then [] else [ colspan_ (Text.pack $ show i) ]
        j' = if j == 1 then [] else [ rowspan_ (Text.pack $ show j) ]

    examplesToHtml :: [Example] -> Html ()
    examplesToHtml l = pre_ [class_ "screen"] (mconcat $ map exampleToHtml l)

    exampleToHtml :: Example -> Html ()
    exampleToHtml (Example expression result) = htmlExample
      where
        htmlExample = htmlPrompt <> htmlExpression <> toHtml (Text.pack $ unlines result)
        htmlPrompt = (code_ [class_ "prompt"] (toHtml (">>> " :: Text)))
        htmlExpression = (strong_ . code_ [class_ "userinput"] . toHtml $ expression ++ "\n")

    makeOrdList :: (ToHtml a) => [(Int, a)] -> Html ()
    makeOrdList items = ol_ (foldMap (\(index, a) -> li_ [value_ (Text.pack . show $ index)] (toHtml a)) items)

-- | We use this intermediate type to transform the input 'Doc' tree
-- in an arbitrary way before rendering, such as grouping some
-- elements. This is effectively a hack to prevent the 'Doc' type
-- from changing if it is possible to recover the layout information
-- we won't need after the fact.
data Hack a id =
  UntouchedDoc (MetaDoc a id)
  | CollapsingHeader (Header (DocH a id)) (MetaDoc a id) Int (Maybe String)
  | HackAppend (Hack a id) (Hack a id)
  deriving Eq

-- | Group things under bold 'DocHeader's together.
toHack :: Int -- ^ Counter for header IDs which serves to assign
              -- unique identifiers within the comment scope
       -> Maybe String
       -- ^ It is not enough to have unique identifier within the
       -- scope of the comment: if two different comments have the
       -- same ID for headers, the collapse/expand behaviour will act
       -- on them both. This serves to make each header a little bit
       -- more unique. As we can't export things with the same names,
       -- this should work more or less fine: it is in fact the
       -- implicit assumption the collapse/expand mechanism makes for
       -- things like ‘Instances’ boxes.
       -> [MetaDoc a id] -> Hack a id
toHack _ _ [] = UntouchedDoc emptyMetaDoc
toHack _ _ [x] = UntouchedDoc x
toHack n nm (MetaDoc { _doc = DocHeader (Header l (DocBold x)) }:xs) =
  let -- Header with dropped bold
      h = Header l x
      -- Predicate for takeWhile, grab everything including ‘smaller’
      -- headers
      p (MetaDoc { _doc = DocHeader (Header l' _) }) = l' > l
      p _ = True
      -- Stuff ‘under’ this header
      r = takeWhile p xs
      -- Everything else that didn't make it under
      r' = drop (length r) xs
      app y [] = y
      app y ys = HackAppend y (toHack (n + 1) nm ys)
  in case r of
      -- No content under this header
      [] -> CollapsingHeader h emptyMetaDoc n nm `app` r'
      -- We got something out, stitch it back together into one chunk
      y:ys -> CollapsingHeader h (foldl metaDocAppend y ys) n nm `app` r'
toHack n nm (x:xs) = HackAppend (UntouchedDoc x) (toHack n nm xs)

-- | Remove ‘top-level’ 'DocAppend's turning them into a flat list.
-- This lends itself much better to processing things in order user
-- might look at them, such as in 'toHack'.
flatten :: MetaDoc a id -> [MetaDoc a id]
flatten MetaDoc { _meta = m, _doc = DocAppend x y } =
  let f z = MetaDoc { _meta = m, _doc = z }
  in flatten (f x) ++ flatten (f y)
flatten x = [x]

-- | Generate the markup needed for collapse to happen. For
-- 'UntouchedDoc' and 'HackAppend' we do nothing more but
-- extract/append the underlying 'Doc' and convert it to 'Html'. For
-- 'CollapsingHeader', we attach extra info to the generated 'Html'
-- that allows us to expand/collapse the content.
hackMarkup :: DocMarkup id (Html ()) -> Maybe Package -> Hack (Wrap (ModuleName, OccName)) id -> Html ()
hackMarkup fmt' currPkg h' =
  let (html, ms) = hackMarkup' fmt' h'
  in html <> renderMeta fmt' currPkg (metaConcat ms)
  where
    hackMarkup' :: DocMarkup id (Html ()) -> Hack (Wrap (ModuleName, OccName)) id
                -> (Html (), [Meta])
    hackMarkup' fmt h = case h of
      UntouchedDoc d -> (markup fmt $ _doc d, [_meta d])
      CollapsingHeader (Header lvl titl) par n nm ->
        let id' = Text.pack $ makeAnchorId $ "ch:" ++ fromMaybe "noid:" nm ++ show n
            col' = collapseControl id' "subheading"
            summary = summary_ [ class_ "hide-when-js-enabled" ] (toHtml (Text.pack "Expand"))
            instTable contents = collapseDetails id' DetailsClosed (summary <> contents)
            lvs :: [(Int, [Attributes] -> Html () -> Html ())]
            lvs = zip [1 .. ] [h1_, h2_, h3_, h4_, h5_, h6_]
            getHeader = fromMaybe caption_ (lookup lvl lvs)
            subCaption = getHeader col' (markup fmt titl)
        in ((subCaption <>) . instTable $ markup fmt (_doc par), [_meta par])
      HackAppend d d' -> let (x, m) = hackMarkup' fmt d
                             (y, m') = hackMarkup' fmt d'
                         in (markupAppend fmt x y, m ++ m')

renderMeta :: DocMarkup id (Html ()) -> Maybe Package -> Meta -> Html ()
renderMeta fmt currPkg (Meta { _version = Just x, _package = pkg }) =
  markupParagraph fmt . markupEmphasis fmt . toHtml $
    "Since: " ++ formatPkgMaybe pkg ++ formatVersion x
  where
    formatVersion v = concat . intersperse "." $ map show v
    formatPkgMaybe (Just p) | Just p /= currPkg = p ++ "-"
    formatPkgMaybe _ = ""
renderMeta _ _ _ = pure ()

-- | Goes through 'hackMarkup' to generate the 'Html' rather than
-- skipping straight to 'markup': this allows us to employ XHtml
-- specific hacks to the tree first.
markupHacked :: DocMarkup (Wrap id) (Html ())
             -> Maybe Package      -- this package
             -> Maybe String
             -> MDoc id
             -> Html ()
markupHacked fmt currPkg n = hackMarkup fmt currPkg . toHack 0 n . flatten

-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).  FIXME: Does this still apply?
docToHtml :: Maybe String  -- ^ Name of the thing this doc is for. See
                           -- comments on 'toHack' for details.
          -> Maybe Package -- ^ Current package
          -> Qualification -> MDoc DocName -> Html ()
docToHtml n pkg qual = markupHacked fmt pkg n . cleanup
  where fmt = parHtmlMarkup qual True (ppWrappedDocName qual Raw)

-- | Same as 'docToHtml' but it doesn't insert the 'anchor' element
-- in links. This is used to generate the Contents box elements.
docToHtmlNoAnchors :: Maybe String  -- ^ See 'toHack'
                   -> Maybe Package -- ^ Current package
                   -> Qualification -> MDoc DocName -> Html ()
docToHtmlNoAnchors n pkg qual = markupHacked fmt pkg n . cleanup
  where fmt = parHtmlMarkup qual False (ppWrappedDocName qual Raw)

origDocToHtml :: Maybe Package -> Qualification -> MDoc Name -> Html ()
origDocToHtml pkg qual = markupHacked fmt pkg Nothing . cleanup
  where fmt = parHtmlMarkup qual True (const (ppWrappedName Raw))


rdrDocToHtml :: Maybe Package -> Qualification -> MDoc RdrName -> Html ()
rdrDocToHtml pkg qual = markupHacked fmt pkg Nothing . cleanup
  where fmt = parHtmlMarkup qual True (const (ppRdrName . unwrap))


docElement :: ([Attributes] -> Html () -> Html ()) -> Html () -> Html ()
docElement el content' =
  if renderText content' == ""
    then el [class_ "doc empty"] (toHtmlRaw ("&nbsp;" :: Text))
    else el [class_ "doc"] content'


docSection :: Maybe Name -- ^ Name of the thing this doc is for
           -> Maybe Package -- ^ Current package
           -> Qualification -> Documentation DocName -> Html ()
docSection n pkg qual =
  maybe (pure ()) (docSection_ n pkg qual) . combineDocumentation


docSection_ :: Maybe Name    -- ^ Name of the thing this doc is for
            -> Maybe Package -- ^ Current package
            -> Qualification -> MDoc DocName -> Html ()
docSection_ n pkg qual =
  docElement div_ . docToHtml (getOccString <$> n) pkg qual


cleanup :: MDoc a -> MDoc a
cleanup = overDoc (markup fmtUnParagraphLists)
  where
    -- If there is a single paragraph, then surrounding it with <P>..</P>
    -- can add too much whitespace in some browsers (eg. IE).  However if
    -- we have multiple paragraphs, then we want the extra whitespace to
    -- separate them.  So we catch the single paragraph case and transform it
    -- here. We don't do this in code blocks as it eliminates line breaks.
    unParagraph :: Doc a -> Doc a
    unParagraph (DocParagraph d) = d
    unParagraph doc              = doc

    fmtUnParagraphLists :: DocMarkup (Wrap a) (Doc a)
    fmtUnParagraphLists = idMarkup {
      markupUnorderedList = DocUnorderedList . map unParagraph,
      markupOrderedList   = DocOrderedList   . map (\(index, a) -> (index, unParagraph a))
      }
