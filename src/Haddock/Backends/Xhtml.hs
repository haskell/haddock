-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml (
  ppHtml, copyHtmlBits,
  ppHtmlIndex, ppHtmlContents,
) where


import Prelude hiding (div)

import Haddock.Backends.Xhtml.Decl
import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Layout
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Themes
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.ModuleTree
import Haddock.Types
import Haddock.Version
import Haddock.Utils
import Text.XHtml hiding ( name, title, p, quote )
import Haddock.GhcUtils

import Control.Exception     ( bracket )
import Control.Monad         ( when, unless )
import Control.Monad.Instances ( ) -- for Functor Either a
import Data.Char             ( toUpper )
import Data.List             ( sortBy, groupBy )
import Data.Maybe
import Foreign.Marshal.Alloc ( allocaBytes )
import System.FilePath hiding ( (</>) )
import System.IO             ( IOMode(..), hClose, hGetBuf, hPutBuf, openFile )
import System.Directory hiding ( copyFile )
import Data.Map              ( Map )
import qualified Data.Map as Map hiding ( Map )
import Data.List             ( intercalate )
import Data.Function
import Data.Ord              ( comparing )

import GHC hiding ( NoLink, moduleInfo )
import Name
import Module


--------------------------------------------------------------------------------
-- * Generating HTML documentation
--------------------------------------------------------------------------------


ppHtml :: String
       -> Maybe String                 -- package
       -> [Interface]
       -> FilePath                     -- destination directory
       -> Maybe (Doc GHC.RdrName)      -- prologue text, maybe
       -> Themes                       -- themes
       -> SourceURLs                   -- the source URL (--source)
       -> WikiURLs                     -- the wiki URL (--wiki)
       -> Maybe String                 -- the contents URL (--use-contents)
       -> Maybe String                 -- the index URL (--use-index)
       -> Bool                         -- whether to use unicode in output (--use-unicode)
       -> IO ()

ppHtml doctitle maybe_package ifaces odir prologue
        themes maybe_source_url maybe_wiki_url
        maybe_contents_url maybe_index_url unicode =  do
  let
        visible_ifaces = filter visible ifaces
        visible i = OptHide `notElem` ifaceOptions i
  when (not (isJust maybe_contents_url)) $
    ppHtmlContents odir doctitle maybe_package
        themes maybe_index_url maybe_source_url maybe_wiki_url
        (map toInstalledIface visible_ifaces)
        False -- we don't want to display the packages in a single-package contents
        prologue

  when (not (isJust maybe_index_url)) $
    ppHtmlIndex odir doctitle maybe_package
      themes maybe_contents_url maybe_source_url maybe_wiki_url
      (map toInstalledIface visible_ifaces)

  mapM_ (ppHtmlModule odir doctitle themes
           maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url unicode) visible_ifaces


copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =
        (bracket (openFile fromFPath ReadMode) hClose $ \hFrom ->
         bracket (openFile toFPath WriteMode) hClose $ \hTo ->
         allocaBytes bufferSize $ \buffer ->
                copyContents hFrom hTo buffer)
        where
                bufferSize = 1024

                copyContents hFrom hTo buffer = do
                        count <- hGetBuf hFrom buffer bufferSize
                        when (count > 0) $ do
                                hPutBuf hTo buffer count
                                copyContents hFrom hTo buffer


copyHtmlBits :: FilePath -> FilePath -> Themes -> IO ()
copyHtmlBits odir libdir themes = do
  let
        libhtmldir = joinPath [libdir, "html"]
        copyCssFile f = do
           copyFile f (combine odir (takeFileName f))
        copyLibFile f = do
           copyFile (joinPath [libhtmldir, f]) (joinPath [odir, f])
  mapM_ copyCssFile (cssFiles themes)
  mapM_ copyLibFile [ jsFile, framesFile ]


headHtml :: String -> Maybe String -> Themes -> Html
headHtml docTitle miniPage themes =
  header << [
    meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"],
    thetitle << docTitle,
    styleSheet themes,
    script ! [src jsFile, thetype "text/javascript"] << noHtml,
    script ! [thetype "text/javascript"]
        -- NB: Within XHTML, the content of script tags needs to be
        -- a <![CDATA[ section. Will break if the miniPage name could
        -- have "]]>" in it!
      << primHtml (
          "//<![CDATA[\nwindow.onload = function () {resetStyle();"
          ++ setSynopsis ++ "};\n//]]>\n")
    ]
  where
    setSynopsis = maybe "" (\p -> "setSynopsis(\"" ++ p ++ "\");") miniPage


srcButton :: SourceURLs -> Maybe Interface -> Maybe Html
srcButton (Just src_base_url, _, _) Nothing =
  Just (anchor ! [href src_base_url] << "Source code")
srcButton (_, Just src_module_url, _) (Just iface) =
  let url = spliceURL (Just $ ifaceOrigFilename iface)
                      (Just $ ifaceMod iface) Nothing Nothing src_module_url
   in Just (anchor ! [href url] << "Source code")
srcButton _ _ =
  Nothing


wikiButton :: WikiURLs -> Maybe Module -> Maybe Html
wikiButton (Just wiki_base_url, _, _) Nothing =
  Just (anchor ! [href wiki_base_url] << "User Comments")

wikiButton (_, Just wiki_module_url, _) (Just mdl) =
  let url = spliceURL Nothing (Just mdl) Nothing Nothing wiki_module_url
   in Just (anchor ! [href url] << "User Comments")

wikiButton _ _ =
  Nothing


contentsButton :: Maybe String -> Maybe Html
contentsButton maybe_contents_url
  = Just (anchor ! [href url] << "Contents")
  where url = maybe contentsHtmlFile id maybe_contents_url


indexButton :: Maybe String -> Maybe Html
indexButton maybe_index_url
  = Just (anchor ! [href url] << "Index")
  where url = maybe indexHtmlFile id maybe_index_url


bodyHtml :: String -> Maybe Interface -> Themes
    -> SourceURLs -> WikiURLs
    -> Maybe String -> Maybe String
    -> Html -> Html
bodyHtml doctitle iface themes
           maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url
           pageContent =
  body << [
    divPackageHeader << [
      sectionName << nonEmpty doctitle,
      unordList (catMaybes [
        srcButton maybe_source_url iface,
        wikiButton maybe_wiki_url (ifaceMod `fmap` iface),
        contentsButton maybe_contents_url,
        indexButton maybe_index_url
        ] ++ [styleMenu themes]) ! [theclass "links"]
      ],
    divContent << pageContent,
    divFooter << paragraph << (
      "Produced by " +++
      (anchor ! [href projectUrl] << toHtml projectName) +++
      (" version " ++ projectVersion)
      )
    ]


moduleInfo :: Interface -> Html
moduleInfo iface =
   let
      info = ifaceInfo iface

      doOneEntry :: (String, (HaddockModInfo GHC.Name) -> Maybe String) -> Maybe (String, String)
      doOneEntry (fieldName, field) = field info >>= \a -> return (fieldName, a)

      entries :: [(String, String)]
      entries = mapMaybe doOneEntry [
         ("Portability",hmi_portability),
         ("Stability",hmi_stability),
         ("Maintainer",hmi_maintainer)
         ]
   in
      case entries of
         [] -> noHtml
         _ -> defList entries ! [theclass "info"]


--------------------------------------------------------------------------------
-- * Generate the module contents
--------------------------------------------------------------------------------


ppHtmlContents
   :: FilePath
   -> String
   -> Maybe String
   -> Themes
   -> Maybe String
   -> SourceURLs
   -> WikiURLs
   -> [InstalledInterface] -> Bool -> Maybe (Doc GHC.RdrName)
   -> IO ()
ppHtmlContents odir doctitle _maybe_package
  themes maybe_index_url
  maybe_source_url maybe_wiki_url ifaces showPkgs prologue = do
  let tree = mkModuleTree showPkgs
         [(instMod iface, toInstalledDescription iface) | iface <- ifaces]
      html =
        headHtml doctitle Nothing themes +++
        bodyHtml doctitle Nothing themes
          maybe_source_url maybe_wiki_url
          Nothing maybe_index_url << [
            ppPrologue doctitle prologue,
            ppModuleTree tree
          ]
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, contentsHtmlFile]) (renderToString html)

  -- XXX: think of a better place for this?
  ppHtmlContentsFrame odir doctitle themes ifaces


ppPrologue :: String -> Maybe (Doc GHC.RdrName) -> Html
ppPrologue _ Nothing = noHtml
ppPrologue title (Just doc) =
  docElement divDescription << (h1 << title +++ rdrDocToHtml doc)


ppModuleTree :: [ModuleTree] -> Html
ppModuleTree ts =
  divModuleList << (sectionName << "Modules" +++ mkNodeList [] "n" ts)


mkNodeList :: [String] -> String -> [ModuleTree] -> Html
mkNodeList ss p ts = case ts of
  [] -> noHtml
  _ -> unordList (zipWith (mkNode ss) ps ts)
  where
    ps = [ p ++ '.' : show i | i <- [(1::Int)..]]


mkNode :: [String] -> String -> ModuleTree -> Html
mkNode ss p (Node s leaf pkg short ts) =
  collBtn +++ htmlModule +++ shortDescr +++ htmlPkg +++ subtree
  where
    collBtn = case ts of
      [] -> noHtml
      _ -> collapsebutton p

    htmlModule = thespan ! [theclass "module" ] <<
      (if leaf
        then ppModule (mkModule (stringToPackageId (fromMaybe "" pkg))
                                       (mkModuleName mdl))
        else toHtml s
      )

    mdl = intercalate "." (reverse (s:ss))

    shortDescr = maybe noHtml origDocToHtml short
    htmlPkg = maybe noHtml (thespan ! [theclass "package"] <<) pkg

    subtree = mkNodeList (s:ss) p ts ! [identifier p]


-- | Turn a module tree into a flat list of full module names.  E.g.,
-- @
--  A
--  +-B
--  +-C
-- @
-- becomes
-- @["A", "A.B", "A.B.C"]@
flatModuleTree :: [InstalledInterface] -> [Html]
flatModuleTree ifaces =
    map (uncurry ppModule' . head)
            . groupBy ((==) `on` fst)
            . sortBy (comparing fst)
            $ mods
  where
    mods = [ (moduleString mdl, mdl) | mdl <- map instMod ifaces ]
    ppModule' txt mdl =
      anchor ! [href ((moduleHtmlFile mdl)), target mainFrameName]
        << toHtml txt


ppHtmlContentsFrame :: FilePath -> String -> Themes
  -> [InstalledInterface] -> IO ()
ppHtmlContentsFrame odir doctitle themes ifaces = do
  let mods = flatModuleTree ifaces
      html =
        headHtml doctitle Nothing themes +++
        miniBody << divModuleList <<
          (sectionName << "Modules" +++
           ulist << [ li ! [theclass "module"] << m | m <- mods ])
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, frameIndexHtmlFile]) (renderToString html)


--------------------------------------------------------------------------------
-- * Generate the index
--------------------------------------------------------------------------------


ppHtmlIndex :: FilePath
            -> String
            -> Maybe String
            -> Themes
            -> Maybe String
            -> SourceURLs
            -> WikiURLs
            -> [InstalledInterface]
            -> IO ()
ppHtmlIndex odir doctitle _maybe_package themes
  maybe_contents_url maybe_source_url maybe_wiki_url ifaces = do
  let html = indexPage split_indices Nothing
              (if split_indices then [] else index)

  createDirectoryIfMissing True odir

  when split_indices $
    mapM_ (do_sub_index index) initialChars

  writeFile (joinPath [odir, indexHtmlFile]) (renderToString html)

  where
    indexPage showLetters ch items =
      headHtml (doctitle ++ " (" ++ indexName ch ++ ")") Nothing themes +++
      bodyHtml doctitle Nothing themes
        maybe_source_url maybe_wiki_url
        maybe_contents_url Nothing << [
          if showLetters then indexInitialLetterLinks else noHtml,
          if null items then noHtml else
            divIndex << [sectionName << indexName ch, buildIndex items]
          ]

    indexName ch = "Index" ++ maybe "" (\c -> " - " ++ [c]) ch

    buildIndex items = table << aboves (map indexElt items)

    -- an arbitrary heuristic:
    -- too large, and a single-page will be slow to load
    -- too small, and we'll have lots of letter-indexes with only one
    --   or two members in them, which seems inefficient or
    --   unnecessarily hard to use.
    split_indices = length index > 150

    indexInitialLetterLinks =
      divAlphabet <<
          unordList [ anchor ! [href (subIndexHtmlFile c)] << [c]
                      | c <- initialChars
                      , any ((==c) . toUpper . head . fst) index ]

    -- todo: what about names/operators that start with Unicode
    -- characters?
    -- Exports beginning with '_' can be listed near the end,
    -- presumably they're not as important... but would be listed
    -- with non-split index!
    initialChars = [ 'A'..'Z' ] ++ ":!#$%&*+./<=>?@\\^|-~" ++ "_"

    do_sub_index this_ix c
      = unless (null index_part) $
          writeFile (joinPath [odir, subIndexHtmlFile c]) (renderToString html)
      where
        html = indexPage True (Just c) index_part
        index_part = [(n,stuff) | (n,stuff) <- this_ix, toUpper (head n) == c]


    index :: [(String, Map GHC.Name [(Module,Bool)])]
    index = sortBy cmp (Map.toAscList full_index)
      where cmp (n1,_) (n2,_) = map toUpper n1 `compare` map toUpper n2

    -- for each name (a plain string), we have a number of original HsNames that
    -- it can refer to, and for each of those we have a list of modules
    -- that export that entity.  Each of the modules exports the entity
    -- in a visible or invisible way (hence the Bool).
    full_index :: Map String (Map GHC.Name [(Module,Bool)])
    full_index = Map.fromListWith (flip (Map.unionWith (++)))
                 (concat (map getIfaceIndex ifaces))

    getIfaceIndex iface =
      [ (getOccString name
         , Map.fromList [(name, [(mdl, name `elem` instVisibleExports iface)])])
         | name <- instExports iface ]
      where mdl = instMod iface

    indexElt :: (String, Map GHC.Name [(Module,Bool)]) -> HtmlTable
    indexElt (str, entities) =
       case Map.toAscList entities of
          [(nm,entries)] ->
              td ! [ theclass "src" ] << toHtml str <->
                          indexLinks nm entries
          many_entities ->
              td ! [ theclass "src" ] << toHtml str <-> td << spaceHtml </>
                  aboves (map doAnnotatedEntity (zip [1..] many_entities))

    doAnnotatedEntity :: (Integer, (Name, [(Module, Bool)])) -> HtmlTable
    doAnnotatedEntity (j,(nm,entries))
          = td ! [ theclass "alt" ] <<
                  toHtml (show j) <+> parens (ppAnnot (nameOccName nm)) <->
                   indexLinks nm entries

    ppAnnot n | not (isValOcc n) = toHtml "Type/Class"
              | isDataOcc n      = toHtml "Data Constructor"
              | otherwise        = toHtml "Function"

    indexLinks nm entries =
       td ! [ theclass "module" ] <<
          hsep (punctuate comma
          [ if visible then
               linkId mdl (Just nm) << toHtml (moduleString mdl)
            else
               toHtml (moduleString mdl)
          | (mdl, visible) <- entries ])


--------------------------------------------------------------------------------
-- * Generate the HTML page for a module
--------------------------------------------------------------------------------


ppHtmlModule
        :: FilePath -> String -> Themes
        -> SourceURLs -> WikiURLs
        -> Maybe String -> Maybe String -> Bool
        -> Interface -> IO ()
ppHtmlModule odir doctitle themes
  maybe_source_url maybe_wiki_url
  maybe_contents_url maybe_index_url unicode iface = do
  let
      mdl = ifaceMod iface
      mdl_str = moduleString mdl
      html =
        headHtml mdl_str (Just $ "mini_" ++ moduleHtmlFile mdl) themes +++
        bodyHtml doctitle (Just iface) themes
          maybe_source_url maybe_wiki_url
          maybe_contents_url maybe_index_url << [
            divModuleHeader << (sectionName << mdl_str +++ moduleInfo iface),
            ifaceToHtml maybe_source_url maybe_wiki_url iface unicode
          ]

  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, moduleHtmlFile mdl]) (renderToString html)
  ppHtmlModuleMiniSynopsis odir doctitle themes iface unicode


ppHtmlModuleMiniSynopsis :: FilePath -> String -> Themes
  -> Interface -> Bool -> IO ()
ppHtmlModuleMiniSynopsis odir _doctitle themes iface unicode = do
  let mdl = ifaceMod iface
      html =
        headHtml (moduleString mdl) Nothing themes +++
        miniBody <<
          (divModuleHeader << sectionName << moduleString mdl +++
           miniSynopsis mdl iface unicode)
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, "mini_" ++ moduleHtmlFile mdl]) (renderToString html)


ifaceToHtml :: SourceURLs -> WikiURLs -> Interface -> Bool -> Html
ifaceToHtml maybe_source_url maybe_wiki_url iface unicode
  = ppModuleContents exports +++
    description +++
    synopsis +++
    divInterface (maybe_doc_hdr +++ bdy)
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    -- todo: if something has only sub-docs, or fn-args-docs, should
    -- it be measured here and thus prevent omitting the synopsis?
    has_doc (ExportDecl _ doc _ _) = isJust (fst doc)
    has_doc (ExportNoDecl _ _) = False
    has_doc (ExportModule _) = False
    has_doc _ = True

    no_doc_at_all = not (any has_doc exports)

    description
          = case ifaceRnDoc iface of
              Nothing -> noHtml
              Just doc -> divDescription $
                            sectionName << "Description" +++ docSection doc

        -- omit the synopsis if there are no documentation annotations at all
    synopsis
      | no_doc_at_all = noHtml
      | otherwise
      = divSynposis $
            sectionName << "Synopsis" +++
            shortDeclList (
                mapMaybe (processExport True linksInfo unicode) exports
            )

        -- if the documentation doesn't begin with a section header, then
        -- add one ("Documentation").
    maybe_doc_hdr
      = case exports of
          [] -> noHtml
          ExportGroup _ _ _ : _ -> noHtml
          _ -> h1 << "Documentation"

    bdy =
      foldr (+++) noHtml $
        mapMaybe (processExport False linksInfo unicode) exports

    linksInfo = (maybe_source_url, maybe_wiki_url)


miniSynopsis :: Module -> Interface -> Bool -> Html
miniSynopsis mdl iface unicode =
    divInterface << mapMaybe (processForMiniSynopsis mdl unicode) exports
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)


processForMiniSynopsis :: Module -> Bool -> ExportItem DocName -> Maybe Html
processForMiniSynopsis mdl unicode (ExportDecl (L _loc decl0) _doc _ _insts) =
  ((divTopDecl <<).(declElem <<)) `fmap` case decl0 of
    TyClD d -> let b = ppTyClBinderWithVarsMini mdl d in case d of
        (TyFamily{}) -> Just $ ppTyFamHeader True False d unicode
        (TyData{tcdTyPats = ps})
          | Nothing <- ps -> Just $ keyword "data" <+> b
          | Just _ <- ps  -> Just $ keyword "data" <+> keyword "instance" <+> b
        (TySynonym{tcdTyPats = ps})
          | Nothing <- ps -> Just $ keyword "type" <+> b
          | Just _ <- ps  -> Just $ keyword "type" <+> keyword "instance" <+> b
        (ClassDecl {})    -> Just $ keyword "class" <+> b
        _ -> Nothing
    SigD (TypeSig (L _ n) (L _ _)) ->
         Just $ ppNameMini mdl (docNameOcc n)
    _ -> Nothing
processForMiniSynopsis _ _ (ExportGroup lvl _id txt) =
  Just $ groupTag lvl << docToHtml txt
processForMiniSynopsis _ _ _ = Nothing


ppNameMini :: Module -> OccName -> Html
ppNameMini mdl nm =
    anchor ! [ href (moduleNameUrl mdl nm)
             , target mainFrameName ]
      << ppBinder' nm


ppTyClBinderWithVarsMini :: Module -> TyClDecl DocName -> Html
ppTyClBinderWithVarsMini mdl decl =
  let n = unLoc $ tcdLName decl
      ns = tyvarNames $ tcdTyVars decl
  in ppTypeApp n ns (ppNameMini mdl . docNameOcc) ppTyName


ppModuleContents :: [ExportItem DocName] -> Html
ppModuleContents exports
  | null sections = noHtml
  | otherwise     = contentsDiv
 where
  contentsDiv = divTableOfContents << (
    sectionName << "Contents" +++
    unordList sections)

  (sections, _leftovers{-should be []-}) = process 0 exports

  process :: Int -> [ExportItem DocName] -> ([Html],[ExportItem DocName])
  process _ [] = ([], [])
  process n items@(ExportGroup lev id0 doc : rest)
    | lev <= n  = ( [], items )
    | otherwise = ( html:secs, rest2 )
    where
        html = linkedAnchor id0 << docToHtml doc +++ mk_subsections ssecs
        (ssecs, rest1) = process lev rest
        (secs,  rest2) = process n   rest1
  process n (_ : rest) = process n rest

  mk_subsections [] = noHtml
  mk_subsections ss = unordList ss


-- we need to assign a unique id to each section heading so we can hyperlink
-- them from the contents:
numberSectionHeadings :: [ExportItem DocName] -> [ExportItem DocName]
numberSectionHeadings exports = go 1 exports
  where go :: Int -> [ExportItem DocName] -> [ExportItem DocName]
        go _ [] = []
        go n (ExportGroup lev _ doc : es)
          = ExportGroup lev (show n) doc : go (n+1) es
        go n (other:es)
          = other : go n es


processExport :: Bool -> LinksInfo -> Bool -> (ExportItem DocName) -> Maybe Html
processExport summary _ _ (ExportGroup lev id0 doc)
  = nothingIf summary $ groupTag lev << namedAnchor id0 << docToHtml doc
processExport summary links unicode (ExportDecl decl doc subdocs insts)
  = processDecl summary $ ppDecl summary links decl doc insts subdocs unicode
processExport summary _ _ (ExportNoDecl y [])
  = processDeclOneLiner summary $ ppDocName y
processExport summary _ _ (ExportNoDecl y subs)
  = processDeclOneLiner summary $ ppDocName y +++ parenList (map ppDocName subs)
processExport summary _ _ (ExportDoc doc)
  = nothingIf summary $ docSection doc
processExport summary _ _ (ExportModule mdl)
  = processDeclOneLiner summary $ toHtml "module" <+> ppModule mdl


nothingIf :: Bool -> a -> Maybe a
nothingIf True _ = Nothing
nothingIf False a = Just a


processDecl :: Bool -> Html -> Maybe Html
processDecl True = Just
processDecl False = Just . divTopDecl


processDeclOneLiner :: Bool -> Html -> Maybe Html
processDeclOneLiner True = Just
processDeclOneLiner False = Just . divTopDecl . declElem


groupTag :: Int -> Html -> Html
groupTag lev
  | lev == 1  = h1
  | lev == 2  = h2
  | lev == 3  = h3
  | otherwise = h4


