
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Backends.Html ( 
  ppHtml, copyHtmlBits, 
  ppHtmlIndex, ppHtmlContents,
  ppHtmlHelpFiles
) where


import Prelude hiding (div)

import Haddock.DocName
import Haddock.Backends.DevHelp
import Haddock.Backends.HH
import Haddock.Backends.HH2
import Haddock.ModuleTree
import Haddock.Types
import Haddock.Version
import Haddock.Utils
import Haddock.Utils.Html
import Haddock.GHC.Utils
import qualified Haddock.Utils.Html as Html

import Control.Exception     ( bracket )
import Control.Monad         ( when, unless )
import Data.Char             ( isUpper, toUpper )
import Data.List             ( sortBy )
import Data.Maybe
import Foreign.Marshal.Alloc ( allocaBytes )
import System.IO             ( IOMode(..), hClose, hGetBuf, hPutBuf, openFile )
import Data.Map              ( Map )
import qualified Data.Map as Map hiding ( Map )

import GHC hiding ( NoLink )
import Name
import Module
import PackageConfig
import RdrName hiding ( Qual )
import SrcLoc   
import FastString            ( unpackFS )
import BasicTypes            ( IPName(..), Boxity(..) )
import Type                  ( Kind )
import Outputable            ( ppr, defaultUserStyle, showSDoc )

-- the base, module and entity URLs for the source code and wiki links.
type SourceURLs = (Maybe String, Maybe String, Maybe String)
type WikiURLs = (Maybe String, Maybe String, Maybe String)

-- -----------------------------------------------------------------------------
-- Generating HTML documentation

ppHtml	:: String
	-> Maybe String				-- package
	-> [Interface]
	-> FilePath			-- destination directory
	-> Maybe (GHC.HsDoc GHC.RdrName)    -- prologue text, maybe
	-> Maybe String		        -- the Html Help format (--html-help)
	-> SourceURLs			-- the source URL (--source)
	-> WikiURLs			-- the wiki URL (--wiki)
	-> Maybe String			-- the contents URL (--use-contents)
	-> Maybe String			-- the index URL (--use-index)
	-> IO ()

ppHtml doctitle maybe_package ifaces odir prologue maybe_html_help_format
	maybe_source_url maybe_wiki_url
	maybe_contents_url maybe_index_url =  do
  let
	visible_ifaces = filter visible ifaces
	visible i = OptHide `notElem` ifaceOptions i

  when (not (isJust maybe_contents_url)) $ 
    ppHtmlContents odir doctitle maybe_package
        maybe_html_help_format maybe_index_url maybe_source_url maybe_wiki_url
        (map toInstalledIface visible_ifaces)
	False -- we don't want to display the packages in a single-package contents
	prologue

  when (not (isJust maybe_index_url)) $ 
    ppHtmlIndex odir doctitle maybe_package maybe_html_help_format
      maybe_contents_url maybe_source_url maybe_wiki_url 
      (map toInstalledIface visible_ifaces)
    
  when (not (isJust maybe_contents_url && isJust maybe_index_url)) $ 
	ppHtmlHelpFiles doctitle maybe_package ifaces odir maybe_html_help_format []

  mapM_ (ppHtmlModule odir doctitle
	   maybe_source_url maybe_wiki_url
	   maybe_contents_url maybe_index_url) visible_ifaces

ppHtmlHelpFiles	
    :: String                   -- doctitle
    -> Maybe String				-- package
	-> [Interface]
	-> FilePath                 -- destination directory
	-> Maybe String             -- the Html Help format (--html-help)
	-> [FilePath]               -- external packages paths
	-> IO ()
ppHtmlHelpFiles doctitle maybe_package ifaces odir maybe_html_help_format pkg_paths =  do
  let
	visible_ifaces = filter visible ifaces
	visible i = OptHide `notElem` ifaceOptions i

  -- Generate index and contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHProject odir doctitle maybe_package visible_ifaces pkg_paths
    Just "mshelp2" -> do
		ppHH2Files      odir maybe_package visible_ifaces pkg_paths
		ppHH2Collection odir doctitle maybe_package
    Just "devhelp" -> ppDevHelpFile odir doctitle maybe_package visible_ifaces
    Just format    -> fail ("The "++format++" format is not implemented")

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


copyHtmlBits :: FilePath -> FilePath -> Maybe FilePath -> IO ()
copyHtmlBits odir libdir maybe_css = do
  let 
	libhtmldir = pathJoin [libdir, "html"]
	css_file = case maybe_css of
			Nothing -> pathJoin [libhtmldir, cssFile]
			Just f  -> f
	css_destination = pathJoin [odir, cssFile]
	copyLibFile f = do
	   copyFile (pathJoin [libhtmldir, f]) (pathJoin [odir, f])
  copyFile css_file css_destination
  mapM_ copyLibFile [ iconFile, plusFile, minusFile, jsFile ]

footer :: HtmlTable
footer = 
  tda [theclass "botbar"] << 
	( toHtml "Produced by" <+> 
	  (anchor ! [href projectUrl] << toHtml projectName) <+>
	  toHtml ("version " ++ projectVersion)
	)
   
srcButton :: SourceURLs -> Maybe Interface -> HtmlTable
srcButton (Just src_base_url, _, _) Nothing =
  topButBox (anchor ! [href src_base_url] << toHtml "Source code")

srcButton (_, Just src_module_url, _) (Just iface) =
  let url = spliceURL (Just $ ifaceOrigFilename iface)
                      (Just $ ifaceMod iface) Nothing Nothing src_module_url
   in topButBox (anchor ! [href url] << toHtml "Source code")

srcButton _ _ =
  Html.emptyTable
 
spliceURL :: Maybe FilePath -> Maybe Module -> Maybe GHC.Name -> 
             Maybe SrcSpan -> String -> String
spliceURL maybe_file maybe_mod maybe_name maybe_loc url = run url
 where
  file = fromMaybe "" maybe_file
  mod = case maybe_mod of
          Nothing           -> ""
          Just mod -> moduleString mod 
  
  (name, kind) =
    case maybe_name of
      Nothing             -> ("","")
      Just n | isValOcc (nameOccName n) -> (escapeStr (getOccString n), "v")
             | otherwise -> (escapeStr (getOccString n), "t")

  line = case maybe_loc of
    Nothing -> ""
    Just span -> show $ srcSpanStartLine span

  run "" = ""
  run ('%':'M':rest) = mod ++ run rest
  run ('%':'F':rest) = file ++ run rest
  run ('%':'N':rest) = name ++ run rest
  run ('%':'K':rest) = kind ++ run rest
  run ('%':'L':rest) = line ++ run rest
  run ('%':'%':rest) = "%" ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'}':rest) = mod ++ run rest
  run ('%':'{':'F':'I':'L':'E':'}':rest)         = file ++ run rest
  run ('%':'{':'N':'A':'M':'E':'}':rest)         = name ++ run rest
  run ('%':'{':'K':'I':'N':'D':'}':rest)         = kind ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'/':'.':'/':c:'}':rest) =
    map (\x -> if x == '.' then c else x) mod ++ run rest

  run ('%':'{':'F':'I':'L':'E':'/':'/':'/':c:'}':rest) =
    map (\x -> if x == '/' then c else x) file ++ run rest

  run ('%':'{':'L':'I':'N':'E':'}':rest)         = line ++ run rest

  run (c:rest) = c : run rest
  
wikiButton :: WikiURLs -> Maybe Module -> HtmlTable
wikiButton (Just wiki_base_url, _, _) Nothing =
  topButBox (anchor ! [href wiki_base_url] << toHtml "User Comments")

wikiButton (_, Just wiki_module_url, _) (Just mod) =
  let url = spliceURL Nothing (Just mod) Nothing Nothing wiki_module_url
   in topButBox (anchor ! [href url] << toHtml "User Comments")

wikiButton _ _ =
  Html.emptyTable

contentsButton :: Maybe String -> HtmlTable
contentsButton maybe_contents_url 
  = topButBox (anchor ! [href url] << toHtml "Contents")
  where url = case maybe_contents_url of
			Nothing -> contentsHtmlFile
			Just url -> url

indexButton :: Maybe String -> HtmlTable
indexButton maybe_index_url 
  = topButBox (anchor ! [href url] << toHtml "Index")
  where url = case maybe_index_url of
			Nothing -> indexHtmlFile
			Just url -> url

simpleHeader :: String -> Maybe String -> Maybe String
             -> SourceURLs -> WikiURLs -> HtmlTable
simpleHeader doctitle maybe_contents_url maybe_index_url
  maybe_source_url maybe_wiki_url = 
  (tda [theclass "topbar"] << 
     vanillaTable << (
       (td << 
  	image ! [src "haskell_icon.gif", width "16", height 16, alt " " ]
       ) <->
       (tda [theclass "title"] << toHtml doctitle) <->
	srcButton maybe_source_url Nothing <->
        wikiButton maybe_wiki_url Nothing <->
	contentsButton maybe_contents_url <-> indexButton maybe_index_url
   ))

pageHeader :: String -> Interface -> String
    -> SourceURLs -> WikiURLs
    -> Maybe String -> Maybe String -> HtmlTable
pageHeader mdl iface doctitle
           maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url =
  (tda [theclass "topbar"] << 
    vanillaTable << (
       (td << 
  	image ! [src "haskell_icon.gif", width "16", height 16, alt " "]
       ) <->
       (tda [theclass "title"] << toHtml doctitle) <->
	srcButton maybe_source_url (Just iface) <->
	wikiButton maybe_wiki_url (Just $ ifaceMod iface) <->
	contentsButton maybe_contents_url <->
	indexButton maybe_index_url
    )
   ) </>
   tda [theclass "modulebar"] <<
	(vanillaTable << (
	  (td << font ! [size "6"] << toHtml mdl) <->
	  moduleInfo iface
	)
    )

moduleInfo :: Interface -> HtmlTable
moduleInfo iface = 
   let
      info = ifaceInfo iface

      doOneEntry :: (String, (GHC.HaddockModInfo GHC.Name) -> Maybe String) -> Maybe HtmlTable
      doOneEntry (fieldName,field) = case field info of
         Nothing -> Nothing
         Just fieldValue -> 
            Just ((tda [theclass "infohead"] << toHtml fieldName)
               <-> (tda [theclass "infoval"]) << toHtml fieldValue)
     
      entries :: [HtmlTable]
      entries = mapMaybe doOneEntry [
         ("Portability",GHC.hmi_portability),
         ("Stability",GHC.hmi_stability),
         ("Maintainer",GHC.hmi_maintainer)
         ]
   in
      case entries of
         [] -> Html.emptyTable
         _ -> tda [align "right"] << narrowTable << (foldl1 (</>) entries)

-- ---------------------------------------------------------------------------
-- Generate the module contents

ppHtmlContents
   :: FilePath
   -> String
   -> Maybe String
   -> Maybe String
   -> Maybe String
   -> SourceURLs
   -> WikiURLs
   -> [InstalledInterface] -> Bool -> Maybe (GHC.HsDoc GHC.RdrName)
   -> IO ()
ppHtmlContents odir doctitle
  maybe_package maybe_html_help_format maybe_index_url
  maybe_source_url maybe_wiki_url ifaces showPkgs prologue = do
  let tree = mkModuleTree showPkgs
         [(instMod mod, toInstalledDescription mod) | mod <- ifaces]
      html = 
	header 
		(documentCharacterEncoding +++
		 thetitle (toHtml doctitle) +++
		 styleSheet +++
		 (script ! [src jsFile, thetype "text/javascript"] $ noHtml)) +++
        body << vanillaTable << (
   	    simpleHeader doctitle Nothing maybe_index_url
                         maybe_source_url maybe_wiki_url </>
	    ppPrologue doctitle prologue </>
	    ppModuleTree doctitle tree </>
	    s15 </>
	    footer
	  )
  writeFile (pathJoin [odir, contentsHtmlFile]) (renderHtml html)
  
  -- Generate contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHContents  odir doctitle maybe_package tree
    Just "mshelp2" -> ppHH2Contents odir doctitle maybe_package tree
    Just "devhelp" -> return ()
    Just format    -> fail ("The "++format++" format is not implemented")

ppPrologue :: String -> Maybe (GHC.HsDoc GHC.RdrName) -> HtmlTable
ppPrologue title Nothing = Html.emptyTable
ppPrologue title (Just doc) = 
  (tda [theclass "section1"] << toHtml title) </>
  docBox (rdrDocToHtml doc)

ppModuleTree :: String -> [ModuleTree] -> HtmlTable
ppModuleTree _ ts = 
  tda [theclass "section1"] << toHtml "Modules" </>
  td << vanillaTable2 << htmlTable
  where
    genTable htmlTable id []     = (htmlTable,id)
    genTable htmlTable id (x:xs) = genTable (htmlTable </> u) id' xs      
      where
        (u,id') = mkNode [] x 0 id

    (htmlTable,_) = genTable emptyTable 0 ts

mkNode :: [String] -> ModuleTree -> Int -> Int -> (HtmlTable,Int)
mkNode ss (Node s leaf pkg short ts) depth id = htmlNode
  where
    htmlNode = case ts of
      [] -> (td_pad_w 1.25 depth << htmlModule  <-> shortDescr <-> htmlPkg,id)
      _  -> (td_w depth << (collapsebutton id_s +++ htmlModule) <-> shortDescr <-> htmlPkg </> 
                (td_subtree << sub_tree), id')

    mod_width = 50::Int {-em-}

    td_pad_w pad depth = 
	tda [thestyle ("padding-left: " ++ show pad ++ "em;" ++
		       "width: " ++ show (mod_width - depth*2) ++ "em")]

    td_w depth = 
	tda [thestyle ("width: " ++ show (mod_width - depth*2) ++ "em")]

    td_subtree =
	tda [thestyle ("padding: 0; padding-left: 2em")]

    shortDescr :: HtmlTable
    shortDescr = case short of
	Nothing -> td empty
	Just doc -> tda [theclass "rdoc"] (origDocToHtml doc)

    htmlModule 
      | leaf      = ppModule (mkModule (stringToPackageId pkgName) 
                                       (mkModuleName mdl)) ""
      | otherwise = toHtml s

    -- ehm.. TODO: change the ModuleTree type
    (htmlPkg, pkgName) = case pkg of
      Nothing -> (td << empty, "")
      Just p  -> (td << toHtml p, p)

    mdl = foldr (++) "" (s' : map ('.':) ss')
    (s':ss') = reverse (s:ss)
	 -- reconstruct the module name
    
    id_s = "n:" ++ show id
    
    (sub_tree,id') = genSubTree emptyTable (id+1) ts
    
    genSubTree :: HtmlTable -> Int -> [ModuleTree] -> (Html,Int)
    genSubTree htmlTable id [] = (sub_tree,id)
      where
        sub_tree = collapsed vanillaTable2 id_s htmlTable
    genSubTree htmlTable id (x:xs) = genSubTree (htmlTable </> u) id' xs      
      where
        (u,id') = mkNode (s:ss) x (depth+1) id

-- The URL for source and wiki links, and the current module
type LinksInfo = (SourceURLs, WikiURLs, Interface)


-- ---------------------------------------------------------------------------
-- Generate the index

ppHtmlIndex :: FilePath
            -> String 
            -> Maybe String
            -> Maybe String
            -> Maybe String
            -> SourceURLs
            -> WikiURLs
            -> [InstalledInterface] 
            -> IO ()
ppHtmlIndex odir doctitle maybe_package maybe_html_help_format
  maybe_contents_url maybe_source_url maybe_wiki_url ifaces = do
  let html = 
        header (documentCharacterEncoding +++
                thetitle (toHtml (doctitle ++ " (Index)")) +++
        styleSheet +++
        (script ! [src jsFile, thetype "text/javascript"] $ noHtml)) +++
        body << vanillaTable << (
            simpleHeader doctitle maybe_contents_url Nothing
                         maybe_source_url maybe_wiki_url </>
        search_box </> index_html
           )

  writeFile (pathJoin [odir, indexHtmlFile]) (renderHtml html)
  
    -- Generate index and contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHIndex  odir maybe_package ifaces
    Just "mshelp2" -> ppHH2Index odir maybe_package ifaces
    Just "devhelp" -> return ()
    Just format    -> fail ("The "++format++" format is not implemented")
 where
  -- colspan 2, marginheight 5
  search_box :: HtmlTable
  search_box = tda [colspan 2, thestyle "padding-top:5px;"] << search
    where
      search :: Html
      search = form ! [strAttr "onsubmit" "full_search(); return false;", action ""] << (
                    "Search: "
                    +++ input ! [identifier "searchbox", strAttr "onkeyup" "quick_search()"]
                    +++ " " +++ input ! [value "Search", thetype "submit"]
                    +++ " " +++ thespan ! [identifier "searchmsg"] << " ")

  index_html = td << setTrClass (table ! [identifier "indexlist", cellpadding 0, cellspacing 5] <<
          aboves (map indexElt index))

  setTrClass :: Html -> Html
  setTrClass (Html xs) = Html $ map f xs
      where
          f (HtmlTag name attrs inner)
               | map toUpper name == "TR" = HtmlTag name (theclass "indexrow":attrs) inner
               | otherwise = HtmlTag name attrs (setTrClass inner)
          f x = x

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
	    tda [ theclass "indexentry" ] << toHtml str <-> 
			indexLinks nm entries
	many_entities ->
	    tda [ theclass "indexentry" ] << toHtml str </> 
		aboves (map doAnnotatedEntity (zip [1..] many_entities))

  doAnnotatedEntity (j,(nm,entries))
	= tda [ theclass "indexannot" ] << 
		toHtml (show j) <+> parens (ppAnnot (nameOccName nm)) <->
		 indexLinks nm entries

  ppAnnot n | not (isValOcc n) = toHtml "Type/Class"
            | isDataOcc n      = toHtml "Data Constructor"
            | otherwise        = toHtml "Function"

  indexLinks nm entries = 
     tda [ theclass "indexlinks" ] << 
	hsep (punctuate comma 
	[ if visible then
	     linkId mod (Just nm) << toHtml (moduleString mod)
	  else
	     toHtml (moduleString mod)
	| (mod, visible) <- entries ])

-- ---------------------------------------------------------------------------
-- Generate the HTML page for a module

ppHtmlModule
	:: FilePath -> String
	-> SourceURLs -> WikiURLs
	-> Maybe String -> Maybe String
	-> Interface -> IO ()
ppHtmlModule odir doctitle
  maybe_source_url maybe_wiki_url
  maybe_contents_url maybe_index_url iface = do
  let 
      mod = ifaceMod iface
      mdl = moduleString mod
      html = 
	header (documentCharacterEncoding +++
		thetitle (toHtml mdl) +++
		styleSheet +++
		(script ! [src jsFile, thetype "text/javascript"] $ noHtml)) +++
        body << vanillaTable << (
	    pageHeader mdl iface doctitle
		maybe_source_url maybe_wiki_url
		maybe_contents_url maybe_index_url </> s15 </>
	    ifaceToHtml maybe_source_url maybe_wiki_url iface </> s15 </>
	    footer
         )
  writeFile (pathJoin [odir, moduleHtmlFile mod]) (renderHtml html)


ifaceToHtml :: SourceURLs -> WikiURLs -> Interface -> HtmlTable
ifaceToHtml maybe_source_url maybe_wiki_url iface
  = abovesSep s15 (contents ++ description: synopsis: maybe_doc_hdr: bdy)
  where
    docMap = ifaceRnDocMap iface
 
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    has_doc (ExportDecl _ doc _) = isJust doc
    has_doc (ExportNoDecl _ _ _) = False
    has_doc (ExportModule _) = False
    has_doc _ = True

    no_doc_at_all = not (any has_doc exports)

    contents = case ppModuleContents exports of
                   Nothing -> []
                   Just x -> [td << vanillaTable << x]

    description
          = case ifaceRnDoc iface of
              Nothing -> Html.emptyTable
              Just doc -> (tda [theclass "section1"] << toHtml "Description") </>
                          docBox (docToHtml doc)

	-- omit the synopsis if there are no documentation annotations at all
    synopsis
      | no_doc_at_all = Html.emptyTable
      | otherwise
      = (tda [theclass "section1"] << toHtml "Synopsis") </>
        s15 </>
            (tda [theclass "body"] << vanillaTable <<
            abovesSep s8 (map (processExport True linksInfo docMap)
            (filter forSummary exports))
        )

	-- if the documentation doesn't begin with a section header, then
	-- add one ("Documentation").
    maybe_doc_hdr
      = case exports of		   
          [] -> Html.emptyTable
          ExportGroup _ _ _ : _ -> Html.emptyTable
          _ -> tda [ theclass "section1" ] << toHtml "Documentation"

    bdy  = map (processExport False linksInfo docMap) exports
    linksInfo = (maybe_source_url, maybe_wiki_url, iface)


ppModuleContents :: [ExportItem DocName] -> Maybe HtmlTable
ppModuleContents exports
  | length sections == 0 = Nothing
  | otherwise            = Just (tda [theclass "section4"] << bold << toHtml "Contents"
  		                 </> td << dlist << concatHtml sections)
 where
  (sections, _leftovers{-should be []-}) = process 0 exports

  process :: Int -> [ExportItem DocName] -> ([Html],[ExportItem DocName])
  process _ [] = ([], [])
  process n items@(ExportGroup lev id0 doc : rest) 
    | lev <= n  = ( [], items )
    | otherwise = ( html:secs, rest2 )
    where
	html = (dterm << linkedAnchor id0 << docToHtml doc)
		 +++ mk_subsections ssecs
	(ssecs, rest1) = process lev rest
	(secs,  rest2) = process n   rest1
  process n (_ : rest) = process n rest

  mk_subsections [] = noHtml
  mk_subsections ss = ddef << dlist << concatHtml ss

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

processExport :: Bool -> LinksInfo -> DocMap -> (ExportItem DocName) -> HtmlTable
processExport _ _ _ (ExportGroup lev id0 doc)
  = ppDocGroup lev (namedAnchor id0 << docToHtml doc)
processExport summary links docMap (ExportDecl decl doc insts)
  = ppDecl summary links decl doc insts docMap
processExport summmary _ _ (ExportNoDecl _ y [])
  = declBox (ppDocName y)
processExport summmary _ _ (ExportNoDecl _ y subs)
  = declBox (ppDocName y <+> parenList (map ppDocName subs))
processExport _ _ _ (ExportDoc doc)
  = docBox (docToHtml doc)
processExport _ _ _ (ExportModule mod)
  = declBox (toHtml "module" <+> ppModule mod "")

forSummary :: (ExportItem DocName) -> Bool
forSummary (ExportGroup _ _ _) = False
forSummary (ExportDoc _)       = False
forSummary _                    = True

ppDocGroup :: Int -> Html -> HtmlTable
ppDocGroup lev doc
  | lev == 1  = tda [ theclass "section1" ] << doc
  | lev == 2  = tda [ theclass "section2" ] << doc
  | lev == 3  = tda [ theclass "section3" ] << doc
  | otherwise = tda [ theclass "section4" ] << doc

declWithDoc :: Bool -> LinksInfo -> SrcSpan -> Name -> Maybe (HsDoc DocName) -> Html -> HtmlTable
declWithDoc True  _     _   _  _          html_decl = declBox html_decl
declWithDoc False links loc nm Nothing    html_decl = topDeclBox links loc nm html_decl
declWithDoc False links loc nm (Just doc) html_decl = 
		topDeclBox links loc nm html_decl </> docBox (docToHtml doc)


ppDecl :: Bool -> LinksInfo -> LHsDecl DocName -> 
          Maybe (HsDoc DocName) -> [InstHead DocName] -> DocMap -> HtmlTable
ppDecl summ links (L loc decl) mbDoc instances docMap = case decl of
  TyClD d@(TyFamily {})          -> ppTyFam summ False links loc mbDoc d
  TyClD d@(TyData {})
    | Nothing <- tcdTyPats d     -> ppDataDecl summ links instances loc mbDoc d
    | Just _  <- tcdTyPats d     -> ppDataInst summ links loc mbDoc d 
  TyClD d@(TySynonym {})
    | Nothing <- tcdTyPats d     -> ppTySyn summ links loc mbDoc d
    | Just _  <- tcdTyPats d     -> ppTyInst summ False links loc mbDoc d
  TyClD d@(ClassDecl {})         -> ppClassDecl summ links instances loc mbDoc docMap d
  SigD (TypeSig (L _ n) (L _ t)) -> ppFunSig summ links loc mbDoc (docNameOrig n) t
  ForD d                         -> ppFor summ links loc mbDoc d
  InstD d                        -> Html.emptyTable

ppFunSig :: Bool -> LinksInfo -> SrcSpan -> Maybe (HsDoc DocName) ->
            Name -> HsType DocName -> HtmlTable
ppFunSig summary links loc mbDoc name typ =
  ppTypeOrFunSig summary links loc name typ mbDoc 
    (ppTypeSig summary (nameOccName name) typ, 
     ppBinder False (nameOccName name), dcolon)


ppTypeOrFunSig :: Bool -> LinksInfo -> SrcSpan -> Name -> HsType DocName ->
                  Maybe (HsDoc DocName) -> (Html, Html, Html) -> HtmlTable
ppTypeOrFunSig summary links loc name typ doc (pref1, pref2, sep)
  | summary || noArgDocs typ = declWithDoc summary links loc name doc pref1
  | otherwise = topDeclBox links loc name pref2 </>
    (tda [theclass "body"] << vanillaTable <<  (
      do_args sep typ </>
        (case doc of
          Just doc -> ndocBox (docToHtml doc)
          Nothing -> Html.emptyTable)
	))
  where 
    noLArgDocs (L _ t) = noArgDocs t
    noArgDocs (HsForAllTy _ _ _ t) = noLArgDocs t
    noArgDocs (HsFunTy (L _ (HsDocTy _ _)) _) = False 
    noArgDocs (HsFunTy _ r) = noLArgDocs r
    noArgDocs (HsDocTy _ _) = False
    noArgDocs _ = True

    do_largs leader (L _ t) = do_args leader t  
    do_args :: Html -> (HsType DocName) -> HtmlTable
    do_args leader (HsForAllTy Explicit tvs lctxt ltype)
      = (argBox (
          leader <+> 
          hsep (keyword "forall" : ppTyVars tvs ++ [dot]) <+>
          ppLContextNoArrow lctxt)
            <-> rdocBox noHtml) </> 
            do_largs darrow ltype
    do_args leader (HsForAllTy Implicit _ lctxt ltype)
      = (argBox (leader <+> ppLContextNoArrow lctxt)
          <-> rdocBox noHtml) </> 
          do_largs darrow ltype
    do_args leader (HsFunTy (L _ (HsDocTy lt ldoc)) r)
      = (argBox (leader <+> ppLType lt) <-> rdocBox (docToHtml (unLoc ldoc)))
          </> do_largs arrow r
    do_args leader (HsFunTy lt r)
      = (argBox (leader <+> ppLType lt) <-> rdocBox noHtml) </> do_largs arrow r
    do_args leader (HsDocTy lt ldoc)
      = (argBox (leader <+> ppLType lt) <-> rdocBox (docToHtml (unLoc ldoc)))
    do_args leader t
      = argBox (leader <+> ppType t) <-> rdocBox (noHtml)


ppTyVars tvs = ppTyNames (tyvarNames tvs)

tyvarNames = map f 
  where f x = docNameOrig . hsTyVarName . unLoc $ x
  
ppFor summary links loc mbDoc (ForeignImport (L _ name) (L _ typ) _)
  = ppFunSig summary links loc mbDoc (docNameOrig name) typ
ppFor _ _ _ _ _ = error "ppFor"

-- we skip type patterns for now
ppTySyn summary links loc mbDoc (TySynonym (L _ name) ltyvars _ ltype) 
  = ppTypeOrFunSig summary links loc n (unLoc ltype) mbDoc 
                   (full, hdr, spaceHtml +++ equals)
  where
    hdr  = hsep ([keyword "type", ppBinder summary occ] ++ ppTyVars ltyvars)
    full = hdr <+> equals <+> ppLType ltype
    n    = docNameOrig name
    occ  = docNameOcc name


ppTypeSig :: Bool -> OccName -> HsType DocName -> Html
ppTypeSig summary nm ty = ppBinder summary nm <+> dcolon <+> ppType ty


ppTyName name
  | isNameSym name = parens (ppName name)
  | otherwise = ppName name


ppTyNames = map ppTyName


--------------------------------------------------------------------------------
-- Type families
--------------------------------------------------------------------------------


ppTyFamHeader :: Bool -> Bool -> TyClDecl DocName -> Html
ppTyFamHeader summary associated decl =

  (case tcdFlavour decl of
     TypeFamily
       | associated -> keyword "type"
       | otherwise  -> keyword "type family"
     DataFamily
       | associated -> keyword "data"
       | otherwise  -> keyword "data family"
  ) <+>

  ppTyClBinderWithVars summary decl <+>

  case tcdKind decl of
    Just kind -> dcolon <+> ppKind kind 
    Nothing -> empty


ppTyFam :: Bool -> Bool -> LinksInfo -> SrcSpan -> Maybe (HsDoc DocName) ->
              TyClDecl DocName -> HtmlTable
ppTyFam summary associated links loc mbDoc decl
  
  | summary = declWithDoc summary links loc name mbDoc 
              (ppTyFamHeader True associated decl)
  
  | associated, isJust mbDoc         = header </> bodyBox << doc 
  | associated                       = header 
  | null instances, isJust mbDoc     = header </> bodyBox << doc
  | null instances                   = header
  | isJust mbDoc                     = header </> bodyBox << (doc </> instancesBit)
  | otherwise                        = header </> bodyBox << instancesBit

  where
    name = docNameOrig . tcdName $ decl

    header = topDeclBox links loc name (ppTyFamHeader summary associated decl)

    doc = ndocBox . docToHtml . fromJust $ mbDoc 

    instId = collapseId name

    instancesBit = instHdr instId </>
  	  tda [theclass "body"] << 
            collapsed thediv instId (
              spacedTable1 << (
                aboves (map (declBox . ppInstHead) instances)
              )
            )

    -- TODO: get the instances
    instances = []


--------------------------------------------------------------------------------
-- Indexed data types
--------------------------------------------------------------------------------


ppDataInst = undefined


--------------------------------------------------------------------------------
-- Indexed newtypes
--------------------------------------------------------------------------------


ppNewTyInst = undefined


--------------------------------------------------------------------------------
-- Indexed types
--------------------------------------------------------------------------------

 
ppTyInst :: Bool -> Bool -> LinksInfo -> SrcSpan -> Maybe (HsDoc DocName) ->
            TyClDecl DocName -> HtmlTable
ppTyInst summary associated links loc mbDoc decl
  
  | summary = declWithDoc summary links loc name mbDoc
              (ppTyInstHeader True associated decl)
  
  | isJust mbDoc = header </> bodyBox << doc 
  | otherwise    = header

  where
    name = docNameOrig . tcdName $ decl

    header = topDeclBox links loc name (ppTyInstHeader summary associated decl)

    doc = case mbDoc of
      Just d -> ndocBox (docToHtml d)
      Nothing -> Html.emptyTable


ppTyInstHeader :: Bool -> Bool -> TyClDecl DocName -> Html
ppTyInstHeader summary associated decl =

  keyword "type instance" <+>

  ppAppNameTypes (tcdName decl) typeArgs
  where
    typeArgs = map unLoc . fromJust . tcdTyPats $ decl


--------------------------------------------------------------------------------
-- Associated Types
--------------------------------------------------------------------------------
    

ppAssocType :: Bool -> LinksInfo -> DocMap -> LTyClDecl DocName -> HtmlTable
ppAssocType summ links docMap (L loc decl) = 
  case decl of
    TyFamily  {} -> ppTyFam summ True links loc doc decl
    TySynonym {} -> ppTySyn summ links loc doc decl
  where
    doc = Map.lookup (docNameOrig $ tcdName decl) docMap


--------------------------------------------------------------------------------
-- TyClDecl helpers
--------------------------------------------------------------------------------


-- | Print a type family / newtype / data / class binder and its variables 
ppTyClBinderWithVars :: Bool -> TyClDecl DocName -> Html
ppTyClBinderWithVars summ decl = 
  ppAppDocNameNames summ (unLoc $ tcdLName decl) (tyvarNames $ tcdTyVars decl)


--------------------------------------------------------------------------------
-- Type applications
--------------------------------------------------------------------------------


-- | Print an application of a DocName and a list of HsTypes
ppAppNameTypes :: DocName -> [HsType DocName] -> Html
ppAppNameTypes n ts = ppTypeApp n ts ppDocName ppParendType


-- | Print an application of a DocName and a list of Names 
ppAppDocNameNames :: Bool -> DocName -> [Name] -> Html
ppAppDocNameNames summ n ns = 
  ppTypeApp n ns (ppBinder summ . docNameOcc) ppTyName


-- | General printing of type applications
ppTypeApp :: DocName -> [a] -> (DocName -> Html) -> (a -> Html) -> Html
ppTypeApp n ts@(t1:t2:rest) ppDN ppT
  | operator, not . null $ rest = parens opApp <+> hsep (map ppT rest)
  | operator                    = opApp
  where
    operator = isNameSym . docNameOrig $ n
    opApp = ppT t1 <+> ppDN n <+> ppT t2

ppTypeApp n ts ppDN ppT = ppDN n <+> hsep (map ppT ts)


-------------------------------------------------------------------------------
-- Contexts 
-------------------------------------------------------------------------------

ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc

ppContextNoArrow :: HsContext DocName -> Html
ppContextNoArrow []  = empty
ppContextNoArrow cxt = pp_hs_context (map unLoc cxt) 

ppContextNoLocs :: [HsPred DocName] -> Html
ppContextNoLocs []  = empty
ppContextNoLocs cxt = pp_hs_context cxt <+> darrow  

ppContext :: HsContext DocName -> Html
ppContext cxt = ppContextNoLocs (map unLoc cxt)

pp_hs_context []  = empty
pp_hs_context [p] = ppPred p
pp_hs_context cxt = parenList (map ppPred cxt) 

ppLPred = ppPred . unLoc


ppPred (HsClassP n ts) = ppAppNameTypes n (map unLoc ts)
-- TODO: find out what happened to the Dupable/Linear distinction
ppPred (HsEqualP t1 t2) = ppLType t1 <+> toHtml "~" <+> ppLType t2
ppPred (HsIParam (IPName n) t) 
  = toHtml "?" +++ ppDocName n <+> dcolon <+> ppLType t


-------------------------------------------------------------------------------
-- Class declarations
-------------------------------------------------------------------------------


ppClassHdr summ lctxt n tvs fds = 
  keyword "class" 
  <+> (if not . null . unLoc $ lctxt then ppLContext lctxt else empty)
  <+> ppAppDocNameNames summ n (tyvarNames $ tvs)
	<+> ppFds fds

ppFds fds =
  if null fds then noHtml else 
	char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
	fundep (vars1,vars2) = hsep (map ppDocName vars1) <+> toHtml "->" <+>
			       hsep (map ppDocName vars2)

ppShortClassDecl :: Bool -> LinksInfo -> TyClDecl DocName -> SrcSpan -> DocMap -> HtmlTable
ppShortClassDecl summary links (ClassDecl lctxt lname tvs fds sigs _ ats _) loc docMap = 
  if null sigs && null ats
    then (if summary then declBox else topDeclBox links loc nm) hdr
    else (if summary then declBox else topDeclBox links loc nm) (hdr <+> keyword "where")
	    </> 
      (
				bodyBox <<
					aboves
					(
						map (ppAssocType summary links docMap) ats ++

						[ ppFunSig summary links loc mbDoc n typ
						| L _ (TypeSig (L _ fname) (L _ typ)) <- sigs
						, let n = docNameOrig fname, let mbDoc = Map.lookup n docMap ] 

					)
				)
  where
    hdr = ppClassHdr summary lctxt (unLoc lname) tvs fds
    nm  = docNameOrig . unLoc $ lname
    


ppClassDecl :: Bool -> LinksInfo -> [InstHead DocName] -> SrcSpan ->
               Maybe (HsDoc DocName) -> DocMap -> TyClDecl DocName -> 
               HtmlTable
ppClassDecl summary links instances loc mbDoc docMap
	decl@(ClassDecl lctxt lname ltyvars lfds lsigs _ ats _)
  | summary = ppShortClassDecl summary links decl loc docMap
  | otherwise = classheader </> bodyBox << (classdoc </> body </> instancesBit)
  where 
    classheader
      | null lsigs = topDeclBox links loc nm hdr
      | otherwise  = topDeclBox links loc nm (hdr <+> keyword "where")

    nm   = docNameOrig . unLoc $ tcdLName decl
    ctxt = unLoc lctxt

    hdr = ppClassHdr summary lctxt (unLoc lname) ltyvars lfds
    
    classdoc = case mbDoc of
      Nothing -> Html.emptyTable
      Just d -> ndocBox (docToHtml d)

    body
      | null lsigs, null ats = Html.emptyTable
      | null ats  = s8 </> methHdr </> bodyBox << methodTable
      | otherwise = s8 </> atHdr </> bodyBox << atTable </> 
                    s8 </> methHdr </> bodyBox << methodTable 
 
    methodTable =
      abovesSep s8 [ ppFunSig summary links loc doc (docNameOrig n) typ
                   | L _ (TypeSig (L _ n) (L _ typ)) <- lsigs
                   , let doc = Map.lookup (docNameOrig n) docMap ]

    atTable = abovesSep s8 $ map (ppAssocType summary links docMap) ats

    instId = collapseId nm
    instancesBit
      | null instances = Html.emptyTable
      | otherwise 
        =  s8 </> instHdr instId </>
           tda [theclass "body"] << 
             collapsed thediv instId (
             spacedTable1 << (
               aboves (map (declBox . ppInstHead) instances)
             ))

ppInstHead :: InstHead DocName -> Html
ppInstHead ([],   n, ts) = ppAppNameTypes n ts 
ppInstHead (ctxt, n, ts) = ppContextNoLocs ctxt <+> ppAppNameTypes n ts 


-- -----------------------------------------------------------------------------
-- Data & newtype declarations


-- TODO: print contexts
ppShortDataDecl :: Bool -> LinksInfo -> SrcSpan -> 
                   Maybe (HsDoc DocName) -> TyClDecl DocName -> Html
ppShortDataDecl summary links loc mbDoc dataDecl 

  | [lcon] <- cons, ResTyH98 <- resTy = 
    ppDataHeader summary dataDecl 
    <+> equals <+> ppShortConstr summary (unLoc lcon)

  | [] <- cons = ppDataHeader summary dataDecl

  | otherwise = vanillaTable << (
      case resTy of 
        ResTyH98 -> dataHeader </> 
          tda [theclass "body"] << vanillaTable << (
            aboves (zipWith doConstr ('=':repeat '|') cons)
          )
        ResTyGADT _ -> dataHeader </> 
          tda [theclass "body"] << vanillaTable << (
            aboves (map doGADTConstr cons)
          )
    )
  
  where
    dataHeader = 
      (if summary then declBox else topDeclBox links loc name)
      ((ppDataHeader summary dataDecl) <+> 
      case resTy of ResTyGADT _ -> keyword "where"; _ -> empty)

    doConstr c con = declBox (toHtml [c] <+> ppShortConstr summary (unLoc con))
    doGADTConstr con = declBox (ppShortConstr summary (unLoc con))

    name      = docNameOrig . unLoc . tcdLName $ dataDecl
    context   = unLoc (tcdCtxt dataDecl)
    newOrData = tcdND dataDecl
    tyVars    = tyvarNames (tcdTyVars dataDecl)
    mbKSig    = tcdKindSig dataDecl
    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons 

ppDataDecl :: Bool -> LinksInfo -> [InstHead DocName] -> 
              SrcSpan -> Maybe (HsDoc DocName) -> TyClDecl DocName -> HtmlTable
ppDataDecl summary links instances loc mbDoc dataDecl
  
  | summary = declWithDoc summary links loc name mbDoc 
              (ppShortDataDecl summary links loc mbDoc dataDecl)
  
  | otherwise
      = (if validTable then (</>) else const) header $
	      tda [theclass "body"] << vanillaTable << (
		      datadoc </> 
		      constrBit </>
		      instancesBit
        )


  where
    name      = docNameOrig . unLoc . tcdLName $ dataDecl
    context   = unLoc (tcdCtxt dataDecl)
    newOrData = tcdND dataDecl
    tyVars    = tyvarNames (tcdTyVars dataDecl)
    mbKSig    = tcdKindSig dataDecl
    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons 
      
    header = topDeclBox links loc name (ppDataHeader summary dataDecl
             <+> whereBit)

    whereBit 
      | null cons = empty 
      | otherwise = case resTy of 
        ResTyGADT _ -> keyword "where"
        _ -> empty                         

    constrTable
      | any isRecCon cons = spacedTable5
      | otherwise         = spacedTable1

    datadoc = case mbDoc of
      Just doc -> ndocBox (docToHtml doc)
      Nothing -> Html.emptyTable

    constrBit 
      | null cons = Html.emptyTable
      | otherwise = constrHdr </> ( 
          tda [theclass "body"] << constrTable << 
	  aboves (map ppSideBySideConstr cons)
        )

    instId = collapseId name

    instancesBit
      | null instances = Html.emptyTable
      | otherwise 
        = instHdr instId </>
	  tda [theclass "body"] << 
          collapsed thediv instId (
            spacedTable1 << (
              aboves (map (declBox . ppInstHead) instances)
            )
          )

    validTable = isJust mbDoc || not (null cons) || not (null instances)


isRecCon lcon = case con_details (unLoc lcon) of 
  RecCon _ -> True
  _ -> False

ppShortConstr :: Bool -> ConDecl DocName -> Html
ppShortConstr summary con = case con_res con of 

  ResTyH98 -> case con_details con of 
    PrefixCon args -> header +++ hsep (ppBinder summary occ : map ppLParendType args)
    RecCon fields -> header +++ ppBinder summary occ <+>
      braces (vanillaTable << aboves (map (ppShortField summary) fields))
    InfixCon arg1 arg2 -> header +++ 
      hsep [ppLParendType arg1, ppBinder summary occ, ppLParendType arg2]    

  ResTyGADT resTy -> case con_details con of 
    PrefixCon args -> doGADTCon args resTy
    RecCon _ -> error "GADT records not suported"
    InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy 
    
  where
    doGADTCon args resTy = ppBinder summary occ <+> dcolon <+> hsep [
                             ppForAll forall ltvs lcontext,
                             ppLType (foldr mkFunTy resTy args) ]

    header   = ppConstrHdr forall tyVars context
    occ      = docNameOcc . unLoc . con_name $ con
    ltvs     = con_qvars con
    tyVars   = tyvarNames ltvs 
    lcontext = con_cxt con
    context  = unLoc (con_cxt con)
    forall   = con_explicit con
    mkFunTy a b = noLoc (HsFunTy a b)

ppConstrHdr :: HsExplicitForAll -> [Name] -> HsContext DocName -> Html
ppConstrHdr forall tvs ctxt
 = (if null tvs then noHtml else ppForall)
   +++
   (if null ctxt then noHtml else ppContextNoArrow ctxt <+> toHtml "=> ")
  where
    ppForall = case forall of 
      Explicit -> keyword "forall" <+> hsep (map ppName tvs) <+> toHtml ". "
      Implicit -> empty

ppSideBySideConstr :: LConDecl DocName -> HtmlTable
ppSideBySideConstr (L _ con) = case con_res con of 
 
  ResTyH98 -> case con_details con of 

    PrefixCon args -> 
      argBox (hsep ((header +++ ppBinder False occ) : map ppLParendType args)) 
      <-> maybeRDocBox mbLDoc  

    RecCon fields -> 
      argBox (header +++ ppBinder False occ) <->
      maybeRDocBox mbLDoc </>
      (tda [theclass "body"] << spacedTable1 <<
      aboves (map ppSideBySideField fields))

    InfixCon arg1 arg2 -> 
      argBox (hsep [header+++ppLParendType arg1, ppBinder False occ, ppLParendType arg2])
      <-> maybeRDocBox mbLDoc
 
  ResTyGADT resTy -> case con_details con of
    PrefixCon args -> doGADTCon args resTy
    RecCon _ -> error "GADT records not supported"
    InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy 

 where 
    doGADTCon args resTy = argBox (ppBinder False occ <+> dcolon <+> hsep [
                               ppForAll forall ltvs (con_cxt con),
                               ppLType (foldr mkFunTy resTy args) ]
                            ) <-> maybeRDocBox mbLDoc


    header  = ppConstrHdr forall tyVars context
    occ     = docNameOcc . unLoc . con_name $ con
    ltvs    = con_qvars con
    tyVars  = tyvarNames (con_qvars con)
    context = unLoc (con_cxt con)
    forall  = con_explicit con
    mbLDoc  = con_doc con
    mkFunTy a b = noLoc (HsFunTy a b)

ppSideBySideField :: ConDeclField DocName -> HtmlTable
ppSideBySideField (ConDeclField (L _ name) ltype mbLDoc) =
  argBox (ppBinder False (docNameOcc name)
    <+> dcolon <+> ppLType ltype) <->
  maybeRDocBox mbLDoc

{-
ppHsFullConstr :: HsConDecl -> Html
ppHsFullConstr (HsConDecl _ nm tvs ctxt typeList doc) = 
     declWithDoc False doc (
	hsep ((ppHsConstrHdr tvs ctxt +++ 
		ppHsBinder False nm) : map ppHsBangType typeList)
      )
ppHsFullConstr (HsRecDecl _ nm tvs ctxt fields doc) =
   td << vanillaTable << (
     case doc of
       Nothing -> aboves [hdr, fields_html]
       Just _  -> aboves [hdr, constr_doc, fields_html]
   )

  where hdr = declBox (ppHsConstrHdr tvs ctxt +++ ppHsBinder False nm)

	constr_doc	
	  | isJust doc = docBox (docToHtml (fromJust doc))
	  | otherwise  = Html.emptyTable

	fields_html = 
	   td << 
	      table ! [width "100%", cellpadding 0, cellspacing 8] << (
		   aboves (map ppFullField (concat (map expandField fields)))
		)
-}

ppShortField :: Bool -> ConDeclField DocName -> HtmlTable
ppShortField summary (ConDeclField (L _ name) ltype _) 
  = tda [theclass "recfield"] << (
      ppBinder summary (docNameOcc name)
      <+> dcolon <+> ppLType ltype
    )

{-
ppFullField :: HsFieldDecl -> Html
ppFullField (HsFieldDecl [n] ty doc) 
  = declWithDoc False doc (
	ppHsBinder False n <+> dcolon <+> ppHsBangType ty
    )
ppFullField _ = error "ppFullField"

expandField :: HsFieldDecl -> [HsFieldDecl]
expandField (HsFieldDecl ns ty doc) = [ HsFieldDecl [n] ty doc | n <- ns ]
-}

-- | Print the LHS of a data\/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: Bool -> TyClDecl DocName -> Html
ppDataHeader summary decl 
  | not (isDataDecl decl) = error "ppDataHeader: illegal argument"
  | otherwise = 
    -- newtype or data
    (if tcdND decl == NewType then keyword "newtype" else keyword "data") <+> 
    -- context
    ppLContext (tcdCtxt decl) <+>
    -- T a b c ..., or a :+: b
    ppTyClBinderWithVars summary decl


-- ----------------------------------------------------------------------------
-- Types and contexts

ppKind k = toHtml $ showSDoc (ppr k)

{-
ppForAll Implicit _ lctxt = ppCtxtPart lctxt
ppForAll Explicit ltvs lctxt = 
  hsep (keyword "forall" : ppTyVars ltvs ++ [dot]) <+> ppCtxtPart lctxt 
-}

ppBang HsStrict = toHtml "!"
ppBang HsUnbox  = toHtml "!!"

tupleParens Boxed   = parenList
tupleParens Unboxed = ubxParenList 
{-
ppType :: HsType DocName -> Html
ppType t = case t of
  t@(HsForAllTy expl ltvs lcontext ltype) -> ppForAllTy t <+> ppLType ltype
  HsTyVar n -> ppDocName n
  HsBangTy HsStrict lt -> toHtml "!" <+> ppLType lt
  HsBangTy HsUnbox lt -> toHtml "!!" <+> ppLType lt
  HsAppTy a b -> ppLType a <+> ppLType b 
  HsFunTy a b -> hsep [ppLType a, toHtml "->", ppLType b]
  HsListTy t -> brackets $ ppLType t
  HsPArrTy t -> toHtml "[:" +++ ppLType t +++ toHtml ":]"
  HsTupleTy Boxed ts -> parenList $ map ppLType ts
  HsTupleTy Unboxed ts -> ubxParenList $ map ppLType ts
  HsOpTy a n b -> ppLType a <+> ppLDocName n <+> ppLType b
  HsParTy t -> parens $ ppLType t
  HsNumTy n -> toHtml (show n)
  HsPredTy p -> ppPred p
  HsKindSig t k -> hsep [ppLType t, dcolon, ppKind k]
  HsSpliceTy _ -> error "ppType"
  HsDocTy t _ -> ppLType t
-}


--------------------------------------------------------------------------------
-- Rendering of HsType 
--------------------------------------------------------------------------------


pREC_TOP = (0 :: Int)   -- type in ParseIface.y in GHC
pREC_FUN = (1 :: Int)   -- btype in ParseIface.y in GHC
                        -- Used for LH arg of (->)
pREC_OP  = (2 :: Int)   -- Used for arg of any infix operator
                        -- (we don't keep their fixities around)
pREC_CON = (3 :: Int)   -- Used for arg of type applicn:
                        -- always parenthesise unless atomic

maybeParen :: Int           -- Precedence of context
           -> Int           -- Precedence of top-level operator
           -> Html -> Html  -- Wrap in parens if (ctxt >= op)
maybeParen ctxt_prec op_prec p | ctxt_prec >= op_prec = parens p
                               | otherwise            = p


ppLTypes       = hsep . map ppLType
ppLParendTypes = hsep . map ppLParendType


ppParendTypes = hsep . map ppParendType


ppLType       = ppType . unLoc
ppLParendType = ppParendType . unLoc


ppType ty       = ppr_mono_ty pREC_TOP ty
ppParendType ty = ppr_mono_ty pREC_CON ty


-- Drop top-level for-all type variables in user style
-- since they are implicit in Haskell

ppForAll exp tvs cxt 
  | show_forall = forall_part <+> ppLContext cxt
  | otherwise   = ppLContext cxt
  where
    show_forall = not (null tvs) && is_explicit
    is_explicit = case exp of {Explicit -> True; Implicit -> False}
    forall_part = hsep (keyword "forall" : ppTyVars tvs) +++ dot 

ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)

ppr_mono_ty ctxt_prec (HsForAllTy exp tvs ctxt ty)
  = maybeParen ctxt_prec pREC_FUN $
    hsep [ppForAll exp tvs ctxt, ppr_mono_lty pREC_TOP ty]

-- gaw 2004
ppr_mono_ty ctxt_prec (HsBangTy b ty)     = ppBang b +++ ppLParendType ty
ppr_mono_ty ctxt_prec (HsTyVar name)      = ppDocName name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   = ppr_fun_ty ctxt_prec ty1 ty2
ppr_mono_ty ctxt_prec (HsTupleTy con tys) = tupleParens con (map ppLType tys)
ppr_mono_ty ctxt_prec (HsKindSig ty kind) = parens (ppr_mono_lty pREC_TOP ty <+> dcolon <+> ppKind kind)
ppr_mono_ty ctxt_prec (HsListTy ty)       = brackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPArrTy ty)       = pabrackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPredTy pred)     = parens (ppPred pred)
ppr_mono_ty ctxt_prec (HsNumTy n)         = toHtml (show n) -- generics only
ppr_mono_ty ctxt_prec (HsSpliceTy s)      = error "ppr_mono_ty-haddock"

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty, ppr_mono_lty pREC_CON arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2)
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 <+> ppr_op <+> ppr_mono_lty pREC_OP ty2
  where
    ppr_op = if not (isSymOcc occName) then quote (ppLDocName op) else ppLDocName op
    occName = docNameOcc . unLoc $ op

ppr_mono_ty ctxt_prec (HsParTy ty)
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty

ppr_mono_ty ctxt_prec (HsDocTy ty doc)
  = ppr_mono_lty ctxt_prec ty

ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty pREC_FUN ty1
        p2 = ppr_mono_lty pREC_TOP ty2
    in
    maybeParen ctxt_prec pREC_FUN $
    hsep [p1, arrow <+> p2]

-- ----------------------------------------------------------------------------
-- Names

ppOccName :: OccName -> Html
ppOccName = toHtml . occNameString

ppRdrName :: RdrName -> Html
ppRdrName = ppOccName . rdrNameOcc

ppLDocName (L _ d) = ppDocName d

ppDocName :: DocName -> Html
ppDocName (Documented name mod) = 
  linkIdOcc mod (Just occName) << ppOccName occName
    where occName = nameOccName name
ppDocName (Undocumented name) = toHtml (getOccString name)

linkTarget :: OccName -> Html
linkTarget n = namedAnchor (anchorNameStr n) << toHtml "" 

ppName :: Name -> Html
ppName name = toHtml (getOccString name)


ppBinder :: Bool -> OccName -> Html
-- The Bool indicates whether we are generating the summary, in which case
-- the binder will be a link to the full definition.
ppBinder True n = linkedAnchor (anchorNameStr n) << ppBinder' n
ppBinder False n = linkTarget n +++ bold << ppBinder' n


ppBinder' :: OccName -> Html
ppBinder' n
  | isVarSym n = parens $ ppOccName n
  | otherwise  = ppOccName n


linkId mod mbName = linkIdOcc mod (fmap nameOccName mbName)


linkIdOcc :: Module -> Maybe OccName -> Html -> Html
linkIdOcc mod mbName = anchor ! [href hr]
  where 
    hr = case mbName of
      Nothing   -> moduleHtmlFile mod
      Just name -> nameHtmlRef mod name

ppModule :: Module -> String -> Html
ppModule mod ref = anchor ! [href ((moduleHtmlFile mod) ++ ref)] 
                   << toHtml (moduleString mod)

-- -----------------------------------------------------------------------------
-- * Doc Markup

parHtmlMarkup :: (a -> Html) -> DocMarkup a Html
parHtmlMarkup ppId = Markup {
  markupParagraph     = paragraph,
  markupEmpty	      = toHtml "",
  markupString        = toHtml,
  markupAppend        = (+++),
  markupIdentifier    = tt . ppId . head,
  markupModule        = \m -> ppModule (mkModuleNoPackage m) "",
  markupEmphasis      = emphasize . toHtml,
  markupMonospaced    = tt . toHtml,
  markupUnorderedList = ulist . concatHtml . map (li <<),
  markupOrderedList   = olist . concatHtml . map (li <<),
  markupDefList       = dlist . concatHtml . map markupDef,
  markupCodeBlock     = pre,
  markupURL	      = \url -> anchor ! [href url] << toHtml url,
  markupAName	      = \aname -> namedAnchor aname << toHtml ""
  }

markupDef (a,b) = dterm << a +++ ddef << b

htmlMarkup = parHtmlMarkup ppDocName
htmlOrigMarkup = parHtmlMarkup ppName
htmlRdrMarkup = parHtmlMarkup ppRdrName

-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).
docToHtml :: GHC.HsDoc DocName -> Html
docToHtml doc = markup htmlMarkup (unParagraph (markup htmlCleanup doc))

origDocToHtml :: GHC.HsDoc GHC.Name -> Html
origDocToHtml doc = markup htmlOrigMarkup (unParagraph (markup htmlCleanup doc))

rdrDocToHtml doc = markup htmlRdrMarkup (unParagraph (markup htmlCleanup doc))

-- If there is a single paragraph, then surrounding it with <P>..</P>
-- can add too much whitespace in some browsers (eg. IE).  However if
-- we have multiple paragraphs, then we want the extra whitespace to
-- separate them.  So we catch the single paragraph case and transform it
-- here.
unParagraph (GHC.DocParagraph d) = d
--NO: This eliminates line breaks in the code block:  (SDM, 6/5/2003)
--unParagraph (DocCodeBlock d) = (DocMonospaced d)
unParagraph doc              = doc

htmlCleanup :: DocMarkup a (GHC.HsDoc a)
htmlCleanup = idMarkup { 
  markupUnorderedList = GHC.DocUnorderedList . map unParagraph,
  markupOrderedList   = GHC.DocOrderedList   . map unParagraph
  } 

-- -----------------------------------------------------------------------------
-- * Misc

hsep :: [Html] -> Html
hsep [] = noHtml
hsep htmls = foldr1 (\a b -> a+++" "+++b) htmls

infixr 8 <+>
(<+>) :: Html -> Html -> Html
a <+> b = Html (getHtmlElements (toHtml a) ++ HtmlString " ": getHtmlElements (toHtml b))

keyword :: String -> Html
keyword s = thespan ! [theclass "keyword"] << toHtml s

equals, comma :: Html
equals = char '='
comma  = char ','

char :: Char -> Html
char c = toHtml [c]

empty :: Html
empty  = noHtml


quote :: Html -> Html
quote h = char '`' +++ h +++ '`'


parens, brackets, braces :: Html -> Html
parens h        = char '(' +++ h +++ char ')'
brackets h      = char '[' +++ h +++ char ']'
pabrackets h    = toHtml "[:" +++ h +++ toHtml ":]"
braces h        = char '{' +++ h +++ char '}'

punctuate :: Html -> [Html] -> [Html]
punctuate _ []     = []
punctuate h (d0:ds) = go d0 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d +++ h) : go e es

abovesSep :: HtmlTable -> [HtmlTable] -> HtmlTable
abovesSep _ []      = Html.emptyTable
abovesSep h (d0:ds) = go d0 ds
                   where
                     go d [] = d
                     go d (e:es) = d </> h </> go e es

parenList :: [Html] -> Html
parenList = parens . hsep . punctuate comma

ubxParenList :: [Html] -> Html
ubxParenList = ubxparens . hsep . punctuate comma

ubxparens :: Html -> Html
ubxparens h = toHtml "(#" +++ h +++ toHtml "#)"

{-
text :: Html
text   = strAttr "TEXT"
-}

-- a box for displaying code
declBox :: Html -> HtmlTable
declBox html = tda [theclass "decl"] << html

-- a box for top level documented names
-- it adds a source and wiki link at the right hand side of the box
topDeclBox :: LinksInfo -> SrcSpan -> Name -> Html -> HtmlTable
topDeclBox ((_,_,Nothing), (_,_,Nothing), _) _ _ html = declBox html
topDeclBox ((_,_,maybe_source_url), (_,_,maybe_wiki_url), iface)
           loc name html =
  tda [theclass "topdecl"] <<
  (        table ! [theclass "declbar"] <<
	    ((tda [theclass "declname"] << html)
             <-> srcLink
             <-> wikiLink)
  )
  where srcLink =
          case maybe_source_url of
            Nothing  -> Html.emptyTable
            Just url -> tda [theclass "declbut"] <<
                          let url' = spliceURL (Just fname) (Just origMod)
                                               (Just name) (Just loc) url
                           in anchor ! [href url'] << toHtml "Source"

        -- for source links, we want to point to the original module,
        -- because only that will have the source.
        origMod = case Map.lookup (nameOccName name) (ifaceEnv iface) of
          Just n -> case nameModule_maybe n of
            Just m -> m
            Nothing -> mod
          _ -> error "This shouldn't happen (topDeclBox)"

        wikiLink =
          case maybe_wiki_url of
            Nothing  -> Html.emptyTable
            Just url -> tda [theclass "declbut"] <<
                          let url' = spliceURL (Just fname) (Just mod)
                                               (Just name) (Just loc) url
                           in anchor ! [href url'] << toHtml "Comments"
  
        mod = ifaceMod iface
        fname = unpackFS (srcSpanFile loc)


-- a box for displaying an 'argument' (some code which has text to the
-- right of it).  Wrapping is not allowed in these boxes, whereas it is
-- in a declBox.
argBox :: Html -> HtmlTable
argBox html = tda [theclass "arg"] << html

-- a box for displaying documentation, 
-- indented and with a little padding at the top
docBox :: Html -> HtmlTable
docBox html = tda [theclass "doc"] << html

-- a box for displaying documentation, not indented.
ndocBox :: Html -> HtmlTable
ndocBox html = tda [theclass "ndoc"] << html

-- a box for displaying documentation, padded on the left a little
rdocBox :: Html -> HtmlTable
rdocBox html = tda [theclass "rdoc"] << html

maybeRDocBox :: Maybe (GHC.LHsDoc DocName) -> HtmlTable
maybeRDocBox Nothing = rdocBox (noHtml)
maybeRDocBox (Just ldoc) = rdocBox (docToHtml (unLoc ldoc))

-- a box for the buttons at the top of the page
topButBox :: Html -> HtmlTable
topButBox html = tda [theclass "topbut"] << html

bodyBox :: Html -> HtmlTable
bodyBox html = tda [theclass "body"] << vanillaTable << html

-- a vanilla table has width 100%, no border, no padding, no spacing
-- a narrow table is the same but without width 100%.
vanillaTable, narrowTable :: Html -> Html
vanillaTable = table ! [theclass "vanilla", cellspacing 0, cellpadding 0]
vanillaTable2 = table ! [theclass "vanilla2", cellspacing 0, cellpadding 0]
narrowTable  = table ! [theclass "narrow",  cellspacing 0, cellpadding 0]

spacedTable1, spacedTable5 :: Html -> Html
spacedTable1 = table ! [theclass "vanilla",  cellspacing 1, cellpadding 0]
spacedTable5 = table ! [theclass "vanilla",  cellspacing 5, cellpadding 0]

constrHdr, methHdr :: HtmlTable
constrHdr  = tda [ theclass "section4" ] << toHtml "Constructors"
methHdr    = tda [ theclass "section4" ] << toHtml "Methods"
atHdr      = tda [ theclass "section4" ] << toHtml "Associated Types"

instHdr :: String -> HtmlTable
instHdr id = 
  tda [ theclass "section4" ] << (collapsebutton id +++ toHtml " Instances")

dcolon, arrow, darrow :: Html
dcolon = toHtml "::"
arrow  = toHtml "->"
darrow = toHtml "=>"
dot    = toHtml "."

s8, s15 :: HtmlTable
s8  = tda [ theclass "s8" ]  << noHtml
s15 = tda [ theclass "s15" ] << noHtml

namedAnchor :: String -> Html -> Html
namedAnchor n = anchor ! [name (escapeStr n)]

--
-- A section of HTML which is collapsible via a +/- button.
--

-- TODO: Currently the initial state is non-collapsed. Change the 'minusFile'
-- below to a 'plusFile' and the 'display:block;' to a 'display:none;' when we
-- use cookies from JavaScript to have a more persistent state.

collapsebutton :: String -> Html
collapsebutton id = 
  image ! [ src minusFile, theclass "coll", onclick ("toggle(this,'" ++ id ++ "')"), alt "show/hide" ]

collapsed :: (HTML a) => (Html -> Html) -> String -> a -> Html
collapsed fn id html =
  fn ! [identifier id, thestyle "display:block;"] << html

-- A quote is a valid part of a Haskell identifier, but it would interfere with
-- the ECMA script string delimiter used in collapsebutton above.
collapseId :: Name -> String
collapseId nm = "i:" ++ escapeStr (getOccString nm)

linkedAnchor :: String -> Html -> Html
linkedAnchor frag = anchor ! [href hr]
   where hr | null frag = ""
            | otherwise = '#': escapeStr frag

documentCharacterEncoding :: Html
documentCharacterEncoding =
   meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"]

styleSheet :: Html
styleSheet =
   thelink ! [href cssFile, rel "stylesheet", thetype "text/css"]
