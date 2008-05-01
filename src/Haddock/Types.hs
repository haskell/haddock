--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


{-# OPTIONS_HADDOCK hide #-}


module Haddock.Types where


import Haddock.GHC.Utils
import Haddock.DocName

import Data.Map (Map)
import qualified Data.Map as Map

import GHC hiding (NoLink)
import Outputable
import OccName
import Name


{-! for DocOption derive: Binary !-}
data DocOption
  = OptHide           -- ^ This module should not appear in the docs
  | OptPrune
  | OptIgnoreExports  -- ^ Pretend everything is exported
  | OptNotHome        -- ^ Not the best place to get docs for things
                      -- exported by this module.
  deriving (Eq, Show)


data ExportItem name

  = ExportDecl {		  		
	
      -- | The original name
      expItemName :: Name, 

      -- | A declaration
      expItemDecl :: LHsDecl name, 
			       
      -- | Maybe a doc comment
      expItemMbDoc :: Maybe (HsDoc name),

      -- | Instances relevant to this declaration
      expItemInstances :: [InstHead name]
	
	  }	-- ^ An exported declaration 
		    
  | ExportNoDecl {
	  -- | The original name
      expItemName :: Name,

      -- | Where to link to
      expItemLinkTarget :: name,

      -- | Subordinate names
      expItemSubs :: [name]

		} -- ^ An exported entity for which we have no 
          -- documentation (perhaps because it resides in
          -- another package)

  | ExportGroup { 

      -- | Section level (1, 2, 3, ... )
      expItemSectionLevel :: Int,

      -- | Section id (for hyperlinks)
      expItemSectionId :: String,     
			
      -- | Section heading text
      expItemSectionText :: HsDoc name

    } -- ^ A section heading

  | ExportDoc (HsDoc name) -- ^ Some documentation

  | ExportModule Module    -- ^ A cross-reference to another module


type InstHead name = ([HsPred name], name, [HsType name])
type ModuleMap     = Map Module Interface
type DocMap        = Map Name (HsDoc DocName)
type LinkEnv       = Map Name Module


-- | This structure holds the module information we get from GHC's 
-- type checking phase
data GhcModule = GhcModule {
   ghcModule         :: Module,
   ghcFilename       :: FilePath,
   ghcMbDocOpts      :: Maybe String,
   ghcHaddockModInfo :: HaddockModInfo Name,
   ghcMbDoc          :: Maybe (HsDoc Name),
   ghcGroup          :: HsGroup Name,
   ghcMbExports      :: Maybe [LIE Name],
   ghcExportedNames  :: [Name],
   ghcNamesInScope   :: [Name],
   ghcInstances      :: [Instance]
}


-- | This is the data structure used to render a Haddock page for a module - it
-- is the "interface" of the module. The core of Haddock lies in creating this 
-- structure (see Haddock.Interface). The structure also holds intermediate
-- data needed during its creation.
data Interface = Interface {

  -- | The documented module
  ifaceMod             :: Module,

  -- | The original filename for this module
  ifaceOrigFilename    :: FilePath,

  -- | Textual information about the module 
  ifaceInfo            :: HaddockModInfo Name,

  -- | The documentation header for this module
  ifaceDoc             :: Maybe (HsDoc Name),

  -- | The renamed documentation header for this module
  ifaceRnDoc           :: Maybe (HsDoc DocName),

  -- | The Haddock options for this module (prune, ignore-exports, etc)
  ifaceOptions         :: [DocOption],

  ifaceExportedDeclMap :: Map Name (LHsDecl Name),
  ifaceDocMap          :: Map Name (HsDoc Name),  
  ifaceRnDocMap        :: Map Name (HsDoc DocName),

  ifaceExportItems     :: [ExportItem Name],
  ifaceRnExportItems   :: [ExportItem DocName],

  -- | Environment mapping exported names to *original* names
	ifaceEnv             :: Map OccName Name,

  -- | All the names that are defined in this module
  ifaceLocals          :: [Name],

  -- | All the names that are exported by this module
  ifaceExports         :: [Name],

  -- | All the visible names exported by this module
  -- For a name to be visible, it has to:
  -- - be exported normally, and not via a full module re-exportation.
  -- - have a declaration in this module or any of it's imports, with the    
  --   exception that it can't be from another package.
  -- Basically, a visible name is a name that will show up in the documentation
  -- for this module.
  ifaceVisibleExports  :: [Name],

  ifaceSubMap          :: Map Name [Name],

  -- | The instances exported by this module
  ifaceInstances       :: [Instance]
}


-- | A smaller version of 'Interface' that we can get from the Haddock
-- interface files.
data InstalledInterface = InstalledInterface {
  instMod            :: Module,
  instInfo           :: HaddockModInfo Name,
  instDocMap         :: Map Name (HsDoc DocName),
  instExports        :: [Name],
  instVisibleExports :: [Name]
}


-- | Convert an 'Interface' to an 'InstalledInterface'
toInstalledIface :: Interface -> InstalledInterface
toInstalledIface interface = InstalledInterface {
  instMod            = ifaceMod            interface,
  instInfo           = ifaceInfo           interface,
  instDocMap         = ifaceRnDocMap       interface,
  instExports        = ifaceExports        interface,
  instVisibleExports = ifaceVisibleExports interface
}


data DocMarkup id a = Markup {
  markupEmpty         :: a,
  markupString        :: String -> a,
  markupParagraph     :: a -> a,
  markupAppend        :: a -> a -> a,
  markupIdentifier    :: [id] -> a,
  markupModule        :: String -> a,
  markupEmphasis      :: a -> a,
  markupMonospaced    :: a -> a,
  markupUnorderedList :: [a] -> a,
  markupOrderedList   :: [a] -> a,
  markupDefList       :: [(a,a)] -> a,
  markupCodeBlock     :: a -> a,
  markupURL           :: String -> a,
  markupAName         :: String -> a
}


-- A monad which collects error messages, locally defined to avoid a dep on mtl

type ErrMsg = String

newtype ErrMsgM a = Writer { runWriter :: (a, [ErrMsg]) }

instance Monad ErrMsgM where
        return a = Writer (a, [])
        m >>= k  = Writer $ let
                (a, w)  = runWriter m
                (b, w') = runWriter (k a)
                in (b, w ++ w')

tell :: [ErrMsg] -> ErrMsgM ()
tell w = Writer ((), w)
