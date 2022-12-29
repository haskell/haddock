{-# language OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.Layout
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Layout (
  miniBody,

  divPackageHeader, divContent, divModuleHeader, divFooter,
  divTableOfContents, divDescription, divSynopsis, divInterface,
  divIndex, divAlphabet, divPackageList, divModuleList,  divContentsList,

  sectionName,
  nonEmptySectionName,

  shortDeclList,
  shortSubDecls,

  divTopDecl,

  SubDecl,
  subArguments,
  subAssociatedTypes,
  subConstructors,
  subPatterns,
  subEquations,
  subFields,
  subInstances, subOrphanInstances,
  subInstHead, subInstDetails, subFamInstDetails,
  subMethods,
  subDefaults,
  subMinimal,

  topDeclElem, declElem,
) where

import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.Types
import Haddock.Utils (makeAnchorId, nameAnchorId)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import GHC.Data.FastString ( unpackFS )
import GHC hiding (a_)
import GHC.Types.Name (nameOccName)
import Lucid.Html5 hiding (Html)
import Lucid (ToHtml(..))

--------------------------------------------------------------------------------
-- * Sections of the document
--------------------------------------------------------------------------------


miniBody :: Html -> Html
miniBody = body_ [id_ "mini"]


sectionDiv :: Text -> Html -> Html
sectionDiv i = div_ [id_ i]


sectionName :: Html -> Html
sectionName = p_ [class_ "caption"]


-- | Make an element that always has at least something (a non-breaking space).
-- If it would have otherwise been empty, then give it the class ".empty".
nonEmptySectionName :: String -> Html
nonEmptySectionName c
  | null c = span_ [class_ "caption empty"] $ spaceHtml
  | otherwise  = span_ [class_ "caption"]       $ toHtml c


divPackageHeader, divContent, divModuleHeader, divFooter,
  divTableOfContents, divDescription, divSynopsis, divInterface,
  divIndex, divAlphabet, divPackageList, divModuleList, divContentsList
    :: Html -> Html

divPackageHeader    = sectionDiv "package-header"
divContent          = sectionDiv "content"
divModuleHeader     = sectionDiv "module-header"
divFooter           = sectionDiv "footer"
divTableOfContents  = sectionDiv "table_-of-contents"
divContentsList     = sectionDiv "contents-list"
divDescription      = sectionDiv "description"
divSynopsis         = sectionDiv "synopsis"
divInterface        = sectionDiv "interface"
divIndex            = sectionDiv "index"
divAlphabet         = sectionDiv "alphabet"
divModuleList       = sectionDiv "module-list"
divPackageList      = sectionDiv "module-list"


--------------------------------------------------------------------------------
-- * Declaration containers
--------------------------------------------------------------------------------


shortDeclList :: [Html] -> Html
shortDeclList items = ul_ $ traverse_ (li_ [class_ "src short"]) items


shortSubDecls :: Bool -> [Html] -> Html
shortSubDecls inst items = ul_ [class_ c] $ traverse_ i items
  where i | inst      = li_ [class_ "inst"]
          | otherwise = li_
        c | inst      = "inst"
          | otherwise = "subs"


divTopDecl :: Html -> Html
divTopDecl = div_ [class_ "top"]


type SubDecl = (Html, Maybe (MDoc DocName), [Html])


divSubDecls ::  Text -> Text -> Maybe Html -> Html
divSubDecls cssClass captionName = maybe mempty wrap
  where
    wrap m = subSection (subCaption <> m)
    subSection = div_ [class_ $ Text.unwords ["subs", cssClass]]
    subCaption = p_ [class_ "caption"] $ toHtml captionName


subDlist :: Maybe Package -> Qualification -> [SubDecl] -> Maybe Html
subDlist _ _ [] = Nothing
subDlist pkg qual decls = Just $ ul_ $ traverse_ subEntry decls
  where
    subEntry (decl, mdoc, subs) =
      li_ $ do
        dfn_ [class_ "src"] $ do
          decl
          docElement div_ $ do
            traverse_ (docToHtml Nothing pkg qual) mdoc
            sequence_ subs


subTable :: Maybe Package -> Qualification -> [SubDecl] -> Maybe Html
subTable _ _ [] = Nothing
subTable pkg qual decls = Just $ table_ $ traverse_ subRow decls
  where
    subRow (decl, mdoc, subs) = tr_ $ do
      td_ [class_ "src"] $ decl
      docElement td_ $ traverse_ (docToHtml Nothing pkg qual) mdoc
      traverse_ td_ subs


-- | Sub table_ with source information (optional).
subTableSrc :: Maybe Package -> Qualification -> LinksInfo -> Bool
            -> [(SubDecl, Maybe Module, Located DocName)] -> Maybe Html
subTableSrc _ _ _ _ [] = Nothing
subTableSrc pkg qual lnks splice decls = Just $ table_ $ traverse_ subRow decls
  where
    subRow ((decl, mdoc, subs), mdl, L loc dn) = tr_ $ do
      td_ [class_ "src clearfix"] $ do
        span_ [class_ "inst-left"] decl <+> linkHtml loc mdl dn
      docElement td_ $ traverse_ (docToHtml Nothing pkg qual) mdoc
      traverse_ td_ subs

    linkHtml :: SrcSpan -> Maybe Module -> DocName -> Html
    linkHtml loc@(RealSrcSpan _ _) mdl dn = links lnks loc splice mdl dn
    linkHtml _ _ _ = noHtml

subBlock :: [Html] -> Maybe Html
subBlock [] = Nothing
subBlock hs = Just $ sequence_ hs


subArguments :: Maybe Package -> Qualification -> [SubDecl] -> Html
subArguments pkg qual = divSubDecls "arguments" ("Arguments" :: Text) . subTable pkg qual


subAssociatedTypes :: [Html] -> Html
subAssociatedTypes = divSubDecls "associated-types" ("Associated Types" :: Text) . subBlock


subConstructors :: Maybe Package -> Qualification -> [SubDecl] -> Html
subConstructors pkg qual = divSubDecls "constructors" "Constructors" . subTable pkg qual

subPatterns :: Maybe Package -> Qualification -> [SubDecl] -> Html
subPatterns pkg qual = divSubDecls "bundled-patterns" "Bundled Patterns" . subTable pkg qual

subFields :: Maybe Package -> Qualification -> [SubDecl] -> Html
subFields pkg qual = divSubDecls "fields" "Fields" . subDlist pkg qual


subEquations :: Maybe Package -> Qualification -> [SubDecl] -> Html
subEquations pkg qual = divSubDecls "equations" "Equations" . subTable pkg qual


-- | Generate collapsible sub table for instance declarations, with source
subInstances :: Maybe Package -> Qualification
             -> String -- ^ Class name, used for a_ generation
             -> LinksInfo -> Bool
             -> [(SubDecl, Maybe Module, Located DocName)] -> Html
subInstances pkg qual nm lnks splice = maybe noHtml wrap . instTable
  where
    wrap contents = subSection (hdr <> collapseDetails ident DetailsOpen (summary <> contents))
    instTable = subTableSrc pkg qual lnks splice
    subSection = div_ [class_ "subs instances"]
    hdr = h4_ (collapseControl ident "instances") $ "Instances"
    summary = summary_ [ class_ "hide-when-js-enabled" ] $ "Instances details"
    ident = Text.pack $ makeAnchorId $ "i:" ++ nm


subOrphanInstances :: Maybe Package -> Qualification
                   -> LinksInfo -> Bool
                   -> [(SubDecl, Maybe Module, Located DocName)] -> Html
subOrphanInstances pkg qual lnks splice  = maybe noHtml wrap . instTable
  where
    wrap = (h1_ "Orphan instances" <>)
    instTable subdecls =
      div_ [id_ ("section." <> ident)] <$> subTableSrc pkg qual lnks splice subdecls
    ident = Text.pack $ makeAnchorId "orphans"


subInstHead :: String -- ^ Instance unique id (for a_ generation)
            -> Html -- ^ Header content (instance name and type)
            -> Html
subInstHead iid hdr =
    expander noHtml <+> hdr
  where
    expander = span_ (collapseControl (instAnchorId iid) "instance")


subInstDetails :: String -- ^ Instance unique id (for a_ generation)
               -> [Html] -- ^ Associated type contents
               -> [Html] -- ^ Method contents (pretty-printed signatures)
               -> Html   -- ^ Source module
               -> Html
subInstDetails iid ats mets mdl =
    subInstSection iid $ (p_ mdl <+> subAssociatedTypes ats <+> subMethods mets)

subFamInstDetails :: String -- ^ Instance unique id (for a_ generation)
                  -> Html   -- ^ Type or data family instance
                  -> Html   -- ^ Source module TODO: use this
                  -> Html
subFamInstDetails iid fi mdl =
    subInstSection iid $ (p_ mdl <+> (div_ [class_ "src"] $ fi))

subInstSection :: String -- ^ Instance unique id (for a_ generation)
               -> Html
               -> Html
subInstSection iid contents = collapseDetails (instAnchorId iid) DetailsClosed (summary <> contents)
  where
    summary = summary_ [ class_ "hide-when-js-enabled" ] "Instance details"

instAnchorId :: String -> Text
instAnchorId iid = Text.pack $ makeAnchorId $ "i:" ++ iid


subMethods :: [Html] -> Html
subMethods = divSubDecls "methods" "Methods" . subBlock

subDefaults :: [Html] -> Html
subDefaults = divSubDecls "default" "" . subBlock

subMinimal :: Html -> Html
subMinimal = divSubDecls "minimal" "Minimal complete definition" . Just . declElem


-- a box for displaying code
declElem :: Html -> Html
declElem = p_ [class_ "src"]


-- a box for top level documented names
-- it adds a source and wiki link at the right hand side of the box
topDeclElem :: LinksInfo -> SrcSpan -> Bool -> [DocName] -> Html -> Html
topDeclElem lnks loc splice names html =
    declElem $ (html <+> (links lnks loc splice Nothing $ head names))
        -- FIXME: is it ok to simply take the first name?

-- | Adds a source and wiki link at the right hand side of the box.
-- Name must be documented, otherwise we wouldn't get here.
links :: LinksInfo -> SrcSpan -> Bool -> Maybe Module -> DocName -> Html
links ((_,_,sourceMap,lineMap), (_,_,maybe_wiki_url)) loc splice mdl' docName@(Documented n mdl) =
  srcLink <+> wikiLink <+> (selfLink `with` [class_ "selflink"] $ "#")
  where selfLink = linkedAnchor (Text.pack $ nameAnchorId (nameOccName (getName docName)))

        srcLink = let nameUrl = Map.lookup origPkg sourceMap
                      lineUrl = Map.lookup origPkg lineMap
                      mUrl | splice    = lineUrl
                                        -- Use the lineUrl as a backup
                           | otherwise = maybe lineUrl Just nameUrl in
          case mUrl of
            Nothing  -> noHtml
            Just url -> let url' = Text.pack $ spliceURL (Just fname) (Just origMod)
                                               (Just n) (Just loc) url
                          in a_ [href_ url', class_ "link"] $ "Source"

        wikiLink =
          case maybe_wiki_url of
            Nothing  -> noHtml
            Just url -> let url' = Text.pack $ spliceURL (Just fname) (Just mdl)
                                               (Just n) (Just loc) url
                          in a_ [href_ url', class_ "link"] $ "Comments"

        -- For source links, we want to point to the original module,
        -- because only that will have the source.
        --
        -- 'mdl'' is a way of "overriding" the module. Without it, instances
        -- will point to the module defining the class/family, which is wrong.
        origMod = fromMaybe (nameModule n) mdl'
        origPkg = moduleUnit origMod

        fname = case loc of
          RealSrcSpan l _ -> unpackFS (srcSpanFile l)
          UnhelpfulSpan _ -> error "links: UnhelpfulSpan"
links _ _ _ _ _ = noHtml
