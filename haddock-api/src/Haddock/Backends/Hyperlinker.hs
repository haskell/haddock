{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Haddock.Backends.Hyperlinker
    ( ppHyperlinkedSource
    , module Haddock.Backends.Hyperlinker.Types
    , module Haddock.Backends.Hyperlinker.Utils
    ) where


import Haddock.Types
import Haddock.Utils (writeUtf8File, out, verbose, Verbosity)
import Haddock.Backends.Hyperlinker.Renderer
import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Utils
import Haddock.Backends.Xhtml.Utils ( renderToString )

import Data.Maybe
import System.Directory
import System.FilePath

import GHC.Iface.Ext.Types  ( pattern HiePath, HieFile(..), HieASTs(..), HieAST(..), SourcedNodeInfo(..) )
import GHC.Iface.Ext.Binary ( readHieFile, hie_file_result, NameCacheUpdater(..))
import GHC.Types.SrcLoc     ( realSrcLocSpan, mkRealSrcLoc )
import Data.Map as M
import GHC.Data.FastString     ( mkFastString )
import GHC.Unit.Module         ( Module, moduleName )
import GHC.Types.Name.Cache    ( initNameCache )
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )


-- | Generate hyperlinked source for given interfaces.
--
-- Note that list of interfaces should also contain interfaces normally hidden
-- when generating documentation. Otherwise this could lead to dead links in
-- produced source.
ppHyperlinkedSource :: Verbosity
                    -> FilePath -- ^ Output directory
                    -> FilePath -- ^ Resource directory
                    -> Maybe FilePath -- ^ Custom CSS file path
                    -> Bool -- ^ Flag indicating whether to pretty-print HTML
                    -> M.Map Module SrcPath -- ^ Paths to sources
                    -> [Interface] -- ^ Interfaces for which we create source
                    -> IO ()
ppHyperlinkedSource verbosity outdir libdir mstyle pretty srcs' ifaces = do
    createDirectoryIfMissing True srcdir
    let cssFile = fromMaybe (defaultCssFile libdir) mstyle
    copyFile cssFile $ srcdir </> srcCssFile
    copyFile (libdir </> "html" </> highlightScript) $
        srcdir </> highlightScript
    mapM_ (ppHyperlinkedModuleSource verbosity srcdir pretty srcs) ifaces
  where
    srcdir = outdir </> hypSrcDir
    srcs = (srcs', M.mapKeys moduleName srcs')

-- | Generate hyperlinked source for particular interface.
ppHyperlinkedModuleSource :: Verbosity -> FilePath -> Bool -> SrcMaps -> Interface -> IO ()
ppHyperlinkedModuleSource verbosity srcdir pretty srcs iface = case ifaceHieFile iface of
    Just hfp -> do
        -- Parse the GHC-produced HIE file
        u <- mkSplitUniqSupply 'a'
        let nc = (initNameCache u [])
            ncu = NCU $ \f -> pure $ snd $ f nc
        HieFile { hie_hs_file = file
                , hie_asts = HieASTs asts
                , hie_types = types
                , hie_hs_src = rawSrc
                } <- hie_file_result
                 <$> (readHieFile ncu hfp)

        -- Get the AST and tokens corresponding to the source file we want
        let fileFs = mkFastString file
            mast | M.size asts == 1 = snd <$> M.lookupMin asts
                 | otherwise        = M.lookup (HiePath (mkFastString file)) asts
            tokens = parse df file rawSrc
            ast = fromMaybe (emptyHieAst fileFs) mast
            fullAst = recoverFullIfaceTypes df types ast

        -- Warn if we didn't find an AST, but there were still ASTs
        if M.null asts
          then pure ()
          else out verbosity verbose $ unwords [ "couldn't find ast for"
                                               , file, show (M.keys asts) ]

        -- Produce and write out the hyperlinked sources
        writeUtf8File path . renderToString pretty . render' fullAst $ tokens
    Nothing -> return ()
  where
    df = ifaceDynFlags iface
    render' = render (Just srcCssFile) (Just highlightScript) srcs
    path = srcdir </> hypSrcModuleFile (ifaceMod iface)

    emptyHieAst fileFs = Node
      { nodeSpan = realSrcLocSpan (mkRealSrcLoc fileFs 1 0)
      , nodeChildren = []
      , sourcedNodeInfo = SourcedNodeInfo mempty
      }

-- | Name of CSS file in output directory.
srcCssFile :: FilePath
srcCssFile = "style.css"

-- | Name of highlight script in output and resource directory.
highlightScript :: FilePath
highlightScript = "highlight.js"

-- | Path to default CSS file.
defaultCssFile :: FilePath -> FilePath
defaultCssFile libdir = libdir </> "html" </> "solarized.css"

