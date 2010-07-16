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
  
  divPackageHeader, divModuleHeader, divFooter,
  divModuleList, divTableOfContents,
  divDescription, divSynposis, divInterface,

  sectionName,
  
  shortDeclList,
  shortSubDecls,
  
  divTopDecl,
  
  SubDecl,
  subArguments,
  subAssociatedTypes,
  subConstructors,
  subFields,
  subInstances,
  subMethods,
  
  topDeclElem, declElem,
  
  vanillaTable, vanillaTable2  
) where

import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Util
import Haddock.Types

import Text.XHtml hiding ( name, title, p, quote )

import FastString            ( unpackFS )
import GHC

-- Sections of the document

miniBody :: Html -> Html
miniBody = body ! [identifier "mini"]

divPackageHeader, divModuleHeader, divFooter :: Html -> Html
divPackageHeader = thediv ! [identifier "package-header"]
divModuleHeader  = thediv ! [identifier "module-header"]
divFooter        = thediv ! [identifier "footer"]

divModuleList, divTableOfContents,
  divDescription, divSynposis, divInterface :: Html -> Html
divModuleList      = thediv ! [identifier "module-list"]
divTableOfContents = thediv ! [identifier "table-of-contents"]
divDescription     = thediv ! [identifier "description"]
divSynposis        = thediv ! [identifier "synopsis"]
divInterface       = thediv ! [identifier "interface"]

-- | The name of a section, used directly after opening a section
sectionName :: Html -> Html
sectionName = paragraph ! [theclass "caption"]


-- | Declaration containers 

shortDeclList :: [Html] -> Html
shortDeclList items = ulist << map (li ! [theclass "src short"] <<) items

shortSubDecls :: [Html] -> Html
shortSubDecls items = ulist ! [theclass "subs"] << map (li <<) items


divTopDecl :: Html -> Html
divTopDecl = thediv ! [theclass "top"]


type SubDecl = (Html, Maybe (Doc DocName), [Html])

divSubDecls :: (HTML a) => String -> a -> Maybe Html -> Html
divSubDecls cssClass captionName = maybe noHtml wrap
  where
    wrap = (subSection <<) . (subCaption +++)
    subSection = thediv ! [theclass $ unwords ["subs", cssClass]]
    subCaption = paragraph ! [theclass "caption"] << captionName

subDlist :: [SubDecl] -> Maybe Html
subDlist [] = Nothing
subDlist decls = Just $ dlist << map subEntry decls
  where
    subEntry (decl, mdoc, subs) =
      dterm ! [theclass "src"] << decl
      +++
      ddef << (fmap docToHtml mdoc `with` subs)
      
    Nothing  `with` [] = spaceHtml
    ma       `with` bs = ma +++ bs

subTable :: [SubDecl] -> Maybe Html
subTable [] = Nothing
subTable decls = Just $ table << aboves (concatMap subRow decls)
  where
    subRow (decl, mdoc, subs) =
      (td ! [theclass "src"] << decl
       <->
       td << nonEmpty (fmap docToHtml mdoc))
      : map (cell . (td <<)) subs

subBlock :: [Html] -> Maybe Html
subBlock [] = Nothing
subBlock hs = Just $ toHtml hs


subArguments :: [SubDecl] -> Html
subArguments = divSubDecls "arguments" "Arguments" . subTable

subAssociatedTypes :: [Html] -> Html
subAssociatedTypes = divSubDecls "associated-types" "Associated Types" . subBlock

subConstructors :: [SubDecl] -> Html
subConstructors = divSubDecls "constructors" "Constructors" . subTable

subFields :: [SubDecl] -> Html
subFields = divSubDecls "fields" "Fields" . subTable

subInstances :: String -> [SubDecl] -> Html
subInstances id_ = divSubDecls "instances" instCaption . instTable
  where
    instCaption = collapsebutton id_ +++ " Instances"
    instTable = fmap (thediv ! [identifier id_] <<) . subTable

subMethods :: [Html] -> Html
subMethods = divSubDecls "methods" "Methods" . subBlock


-- a box for displaying code
declElem :: Html -> Html
declElem = paragraph ! [theclass "src"]

-- a box for top level documented names
-- it adds a source and wiki link at the right hand side of the box
topDeclElem :: LinksInfo -> SrcSpan -> DocName -> Html -> Html
topDeclElem ((_,_,maybe_source_url), (_,_,maybe_wiki_url)) loc name html = 
    declElem << (html +++ srcLink +++ wikiLink)
  where srcLink =
          case maybe_source_url of
            Nothing  -> noHtml
            Just url -> let url' = spliceURL (Just fname) (Just origMod)
                                               (Just n) (Just loc) url
                          in anchor ! [href url', theclass "link"] << "Source"

        wikiLink =
          case maybe_wiki_url of
            Nothing  -> noHtml
            Just url -> let url' = spliceURL (Just fname) (Just mdl)
                                               (Just n) (Just loc) url
                          in anchor ! [href url', theclass "link"] << "Comments"
  
        -- For source links, we want to point to the original module,
        -- because only that will have the source.  
        -- TODO: do something about type instances. They will point to
        -- the module defining the type family, which is wrong.
        origMod = nameModule n

        -- Name must be documented, otherwise we wouldn't get here
        Documented n mdl = name

        fname = unpackFS (srcSpanFile loc)



-- a vanilla table has width 100%, no border, no padding, no spacing
vanillaTable, vanillaTable2 :: Html -> Html
vanillaTable  = table ! [theclass "vanilla",  cellspacing 0, cellpadding 0]
vanillaTable2 = table ! [theclass "vanilla2", cellspacing 0, cellpadding 0]
