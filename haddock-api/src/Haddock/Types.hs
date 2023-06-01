{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-} -- Note [Pass sensitive types]

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Types
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskellorg
-- Stability   :  experimental
-- Portability :  portable
--
-- Types that are commonly used through-out Haddock. Some of the most
-- important types are defined here, like 'Interface' and 'DocName'.
-----------------------------------------------------------------------------
module Haddock.Types (
  module Haddock.Types
  , HsDocString, LHsDocString
  , Fixity(..)
  , module Documentation.Haddock.Types
 ) where

import Control.DeepSeq
import Control.Exception (throw)
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Typeable (Typeable)
import Data.Map (Map)
import Data.Data (Data)
import qualified Data.Set as Set
import Documentation.Haddock.Types
import qualified GHC.Data.Strict as Strict
import GHC.Types.Fixity (Fixity(..))
import GHC.Types.Name (stableNameCmp)
import GHC.Types.Name.Reader (RdrName(..))
import GHC.Types.SourceText (SourceText(..))
import GHC.Types.SrcLoc (BufSpan(..), BufPos(..))
import GHC.Types.Var (Specificity)

import GHC
import GHC.Driver.Session (Language)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.Name.Occurrence
import GHC.Utils.Outputable

-----------------------------------------------------------------------------
-- * Convenient synonyms
-----------------------------------------------------------------------------


type IfaceMap      = Map Module Interface
type InstIfaceMap  = Map Module InstalledInterface  -- TODO: rename
type DocMap a      = Map Name (MDoc a)
type ArgMap a      = Map Name (Map Int (MDoc a))
type SubMap        = Map Name [Name]
type DeclMap       = Map Name DeclMapEntry
type InstMap       = Map RealSrcSpan Name
type FixMap        = Map Name Fixity
type DocPaths      = (FilePath, Maybe FilePath) -- paths to HTML and sources
type WarningMap    = Map Name (Doc Name)


-----------------------------------------------------------------------------
-- * Interfaces and Interface creation
-----------------------------------------------------------------------------

-- | 'Interface' holds all information used to render a single Haddock page.
-- It represents the /interface/ of a module. The core business of Haddock
-- lies in creating this structure. Note that the record contains some fields
-- that are only used to create the final record, and that are not used by the
-- backends.
data Interface = Interface
  {
    -- | The module behind this interface.
    ifaceMod             :: !Module

    -- | Is this a signature?
  , ifaceIsSig           :: !Bool

    -- | Textual information about the module.
  , ifaceInfo            :: !(HaddockModInfo Name)

    -- | Documentation header.
  , ifaceDoc             :: !(Documentation Name)

    -- | Documentation header with cross-reference information.
  , ifaceRnDoc           :: !(Documentation DocName)

    -- | Haddock options for this module (prune, ignore-exports, etc).
  , ifaceOptions         :: [DocOption]

    -- | Documentation of declarations originating from the module (including
    -- subordinates).
  , ifaceDocMap          :: !(DocMap Name)
  , ifaceArgMap          :: !(ArgMap Name)

    -- | The names of all the default methods for classes defined in this module
  , ifaceDefMeths        :: !([(OccName, Name)])

  , ifaceFixMap          :: !(Map Name Fixity)

  , ifaceExportItems     :: [ExportItem GhcRn]
  , ifaceRnExportItems   :: [ExportItem DocNameI]

    -- | All names exported by the module.
  , ifaceExports         :: [Name]

    -- | All \"visible\" names exported by the module.
    -- A visible name is a name that will show up in the documentation of the
    -- module.
    --
    -- Names from modules that are entirely re-exported don't count as visible.
  , ifaceVisibleExports  :: [Name]

    -- | Instances exported by the module.
  , ifaceInstances       :: [HaddockClsInst]

    -- | Orphan instances
  , ifaceOrphanInstances   :: [DocInstance GhcRn]
  , ifaceRnOrphanInstances :: [DocInstance DocNameI]

    -- | The number of haddockable and haddocked items in the module, as a
    -- tuple. Haddockable items are the exports and the module itself.
  , ifaceHaddockCoverage :: (Int, Int)

    -- | Warnings for things defined in this module.
  , ifaceWarningMap :: WarningMap

    -- | Tokenized source code of module (available if Haddock is invoked with
    -- source generation flag).
  , ifaceHieFile ::  !FilePath
  , ifaceDynFlags :: !DynFlags
  }

-- | A subset of the fields of 'Interface' that we store in the interface
-- files.
data InstalledInterface = InstalledInterface
  {
    -- | The module represented by this interface.
    instMod              :: Module

    -- | Is this a signature?
  , instIsSig            :: Bool

    -- | Textual information about the module.
  , instInfo             :: HaddockModInfo Name

    -- | Documentation of declarations originating from the module (including
    -- subordinates).
  , instDocMap           :: DocMap Name

  , instArgMap           :: ArgMap Name

    -- | The names of all the default methods for classes defined in this module
  , instDefMeths         :: [(OccName,Name)]

    -- | All names exported by this module.
  , instExports          :: [Name]

    -- | All \"visible\" names exported by the module.
    -- A visible name is a name that will show up in the documentation of the
    -- module.
  , instVisibleExports   :: [Name]

    -- | Haddock options for this module (prune, ignore-exports, etc).
  , instOptions          :: [DocOption]

  , instFixMap           :: Map Name Fixity
  }

-- | Convert an 'Interface' to an 'InstalledInterface'
toInstalledIface :: Interface -> InstalledInterface
toInstalledIface interface = InstalledInterface
  { instMod              = interface.ifaceMod
  , instIsSig            = interface.ifaceIsSig
  , instInfo             = interface.ifaceInfo
  , instDocMap           = interface.ifaceDocMap
  , instArgMap           = interface.ifaceArgMap
  , instExports          = interface.ifaceExports
  , instVisibleExports   = interface.ifaceVisibleExports
  , instOptions          = interface.ifaceOptions
  , instFixMap           = interface.ifaceFixMap
  , instDefMeths         = interface.ifaceDefMeths

  }


-- | A monad in which we create Haddock interfaces. Not to be confused with
-- `GHC.Tc.Types.IfM` which is used to write GHC interfaces.
--
-- In the past `createInterface` was running in the `Ghc` monad but proved hard
-- to sustain as soon as we moved over for Haddock to be a plugin. Also abstracting
-- over the Ghc specific clarifies where side effects happen.
newtype IfM m a = IfM { unIfM :: StateT (IfEnv m) m a }

deriving newtype instance Functor m                => Functor (IfM m)
deriving newtype instance (Monad m, Applicative m) => Applicative (IfM m)
deriving newtype instance Monad m                  => Monad (IfM m)
deriving newtype instance MonadIO m                => MonadIO (IfM m)
deriving newtype instance Monad m                  => MonadState (IfEnv m) (IfM m)

-- | Interface creation environment. The name sets are used primarily during
-- processing of doc strings to avoid emitting the same type of warning for the
-- same name twice. This was previously done using a Writer monad and then
-- nubbing the list of warning messages after accumulation. This new approach
-- was implemented to avoid the nubbing of potentially large lists of strings.
data IfEnv m = IfEnv
  {
    -- | Lookup names in the environment.
    ifeLookupName :: Name -> m (Maybe TyThing)

    -- | Names which we have warned about for being out of scope
  , ifeOutOfScopeNames :: !(Set.Set String)

    -- | Names which we have warned about for being ambiguous
  , ifeAmbiguousNames  :: !(Set.Set String)

    -- | Named which we have warned about for being inappropriately namespaced
    -- as values
  , ifeInvalidValues   :: !(Set.Set String)
  }

-- | Run an `IfM` action.
runIfM
  :: (Monad m)
  -- | Lookup a global name in the current session. Used in cases
  -- where declarations don't
  => (Name -> m (Maybe TyThing))
  -- | The action to run.
  -> IfM m a
  -- | Result and accumulated error/warning messages.
  -> m a
runIfM lookup_name action = do
  let
    if_env = IfEnv
      {
        ifeLookupName      = lookup_name
      , ifeOutOfScopeNames = Set.empty
      , ifeAmbiguousNames  = Set.empty
      , ifeInvalidValues   = Set.empty
      }
  evalStateT (unIfM action) if_env

-- | Look up a name in the current environment
lookupName :: Monad m => Name -> IfM m (Maybe TyThing)
lookupName name = IfM $ do
  lookup_name <- gets ifeLookupName
  lift (lookup_name name)

-- | Very basic logging function that simply prints to stdout
warn :: MonadIO m => String -> IfM m ()
warn msg = liftIO $ putStrLn msg

-----------------------------------------------------------------------------
-- * Export items & declarations
-----------------------------------------------------------------------------


data ExportItem name

  -- | An exported declaration.
  = ExportDecl (XExportDecl name)

  -- | An exported entity for which we have no documentation (perhaps because it
  -- resides in another package).
  | ExportNoDecl
      { expItemName :: !(IdP name)

        -- | Subordinate names.
      , expItemSubs :: [IdP name]
      }

  -- | A section heading.
  | ExportGroup
      {
        -- | Section level (1, 2, 3, ...).
        expItemSectionLevel :: !Int

        -- | Section id (for hyperlinks).
      , expItemSectionId :: !String

        -- | Section heading text.
      , expItemSectionText :: !(Doc (IdP name))
      }

  -- | Some documentation.
  | ExportDoc !(MDoc (IdP name))

  -- | A cross-reference to another module.
  | ExportModule !Module

-- | A type family mapping a name type index to types of export declarations.
-- The pre-renaming type index ('GhcRn') is mapped to the type of export
-- declarations which do not include Hoogle output ('ExportD'), since Hoogle output is
-- generated during the Haddock renaming step. The post-renaming type index
-- ('DocNameI') is mapped to the type of export declarations which do include
-- Hoogle output ('RnExportD').
type family XExportDecl x where
  XExportDecl GhcRn    = ExportD GhcRn
  XExportDecl DocNameI = RnExportD

-- | Represents an export declaration that Haddock has discovered to be exported
-- from a module. The @name@ index indicated whether the declaration has been
-- renamed such that each 'Name' points to it's optimal link destination.
data ExportD name = ExportD
      {
        -- | A declaration.
        expDDecl :: !(LHsDecl name)

        -- | Bundled patterns for a data type declaration
      , expDPats :: [(HsDecl name, DocForDecl (IdP name))]

        -- | Maybe a doc comment, and possibly docs for arguments (if this
        -- decl is a function or type-synonym).
      , expDMbDoc :: !(DocForDecl (IdP name))

        -- | Subordinate names, possibly with documentation.
      , expDSubDocs :: [(IdP name, DocForDecl (IdP name))]

        -- | Instances relevant to this declaration, possibly with
        -- documentation.
      , expDInstances :: [DocInstance name]

        -- | Fixity decls relevant to this declaration (including subordinates).
      , expDFixities :: [(IdP name, Fixity)]

        -- | Whether the ExportD is from a TH splice or not, for generating
        -- the appropriate type of Source link.
      , expDSpliced :: !Bool
      }

-- | Represents export declarations that have undergone renaming such that every
-- 'Name' in the declaration points to an optimal link destination. Since Hoogle
-- output is also generated during the renaming step, each declaration is also
-- attached to its Hoogle textual database entries, /if/ Hoogle output is
-- enabled and the module is not hidden in the generated documentation using the
-- @{-# OPTIONS_HADDOCK hide #-}@ pragma.
data RnExportD = RnExportD
      {
        -- | The renamed export declaration
        rnExpDExpD :: !(ExportD DocNameI)

      -- | If Hoogle textbase (textual database) output is enabled, the text
      -- output lines for this declaration. If Hoogle output is not enabled, the
      -- list will be empty.
      , rnExpDHoogle :: [String]
      }

data Documentation name = Documentation
  { documentationDoc     :: Maybe (MDoc name)
  , documentationWarning :: Maybe (Doc name)
  } deriving Functor

instance NFData name => NFData (Documentation name) where
  rnf (Documentation d w) = d `deepseq` w `deepseq` ()

-- | Arguments and result are indexed by Int, zero-based from the left,
-- because that's the easiest to use when recursing over types.
type FnArgsDoc name = Map Int (MDoc name)
type DocForDecl name = (Documentation name, FnArgsDoc name)

noDocForDecl :: DocForDecl name
noDocForDecl = (Documentation Nothing Nothing, mempty)

-- | As we build the declaration map, we really only care to track whether we
-- have only seen a value declaration for a 'Name', or anything else. This type
-- is used to represent those cases. If the only declaration attached to a
-- 'Name' is a 'ValD', we will consult the GHC interface file to determine the
-- type of the value, and attach the 'SrcSpan' from the 'EValD' constructor to
-- it. If we see any other type of declaration for the 'Name', we can just use
-- it.
--
-- This type saves us from storing /every/ declaration we see for a given 'Name'
-- in the map, which is unnecessary and very problematic for overall memory
-- usage.
data DeclMapEntry
    = EValD !SrcSpan
    | EOther (LHsDecl GhcRn)

instance Semigroup DeclMapEntry where
  (EValD _)   <> e         = e
  e           <> _         = e

-- | Transform a declaration into a 'DeclMapEntry'. If it is a 'ValD'
-- declaration, only the source location will be noted (since that is all we
-- care to store in the 'DeclMap' due to the way top-level bindings with no type
-- signatures are handled). Otherwise, the entire declaration will be kept.
toDeclMapEntry :: LHsDecl GhcRn -> DeclMapEntry
toDeclMapEntry (L l (ValD _ _)) = EValD (locA l)
toDeclMapEntry d                = EOther d

-----------------------------------------------------------------------------
-- * Cross-referencing
-----------------------------------------------------------------------------


-- | Type of environment used to cross-reference identifiers in the syntax.
type LinkEnv = Map Name Module

-- | An 'RdrName' tagged with some type/value namespace information.
data NsRdrName = NsRdrName
  { namespace :: !Namespace
  , rdrName :: !RdrName
  }

instance NFData NsRdrName where
  rnf (NsRdrName ns rdrN) = ns `seq` rdrN `deepseq` ()

-- | Extends 'Name' with cross-reference information.
data DocName
  = Documented Name Module
     -- ^ This thing is part of the (existing or resulting)
     -- documentation. The 'Module' is the preferred place
     -- in the documentation to refer to.
  | Undocumented Name
     -- ^ This thing is not part of the (existing or resulting)
     -- documentation, as far as Haddock knows.
  deriving (Eq, Data)

data DocNameI

type instance NoGhcTc DocNameI = DocNameI

type instance IdP DocNameI = DocName

instance CollectPass DocNameI where
  collectXXPat _ ext = dataConCantHappen ext
  collectXXHsBindsLR ext = dataConCantHappen ext
  collectXSplicePat _ ext = dataConCantHappen ext

instance NamedThing DocName where
  getName (Documented name _) = name
  getName (Undocumented name) = name

-- | Useful for debugging
instance Outputable DocName where
  ppr = ppr . getName

instance OutputableBndr DocName where
  pprBndr _ = ppr . getName
  pprPrefixOcc = pprPrefixOcc . getName
  pprInfixOcc = pprInfixOcc . getName

class NamedThing name => SetName name where

    setName :: Name -> name -> name


instance SetName Name where

    setName name' _ = name'


instance SetName DocName where

    setName name' (Documented _ mdl) = Documented name' mdl
    setName name' (Undocumented _) = Undocumented name'

-- | Adds extra "wrapper" information to a name.
--
-- This is to work around the fact that most name types in GHC ('Name', 'RdrName',
-- 'OccName', ...) don't include backticks or parens.
data Wrap n
  = Unadorned { unwrap :: n  }     -- ^ don't do anything to the name
  | Parenthesized { unwrap :: n }  -- ^ add parentheses around the name
  | Backticked { unwrap :: n }     -- ^ add backticks around the name
  deriving (Show, Functor, Foldable, Traversable)

instance NFData n => NFData (Wrap n) where
  rnf w = case w of
    Unadorned n     -> rnf n
    Parenthesized n -> rnf n
    Backticked n    -> rnf n

-- | Useful for debugging
instance Outputable n => Outputable (Wrap n) where
  ppr (Unadorned n)     = ppr n
  ppr (Parenthesized n) = hcat [ char '(', ppr n, char ')' ]
  ppr (Backticked n)    = hcat [ char '`', ppr n, char '`' ]

showWrapped :: (a -> String) -> Wrap a -> String
showWrapped f (Unadorned n) = f n
showWrapped f (Parenthesized n) = "(" ++ f n ++ ")"
showWrapped f (Backticked n) = "`" ++ f n ++ "`"

instance HasOccName DocName where

    occName = occName . getName

-----------------------------------------------------------------------------
-- * Instances
-----------------------------------------------------------------------------

data HaddockClsInst = HaddockClsInst
    { haddockClsInstPprHoogle :: Maybe String
    , haddockClsInstName      :: Name
    , haddockClsInstClsName   :: Name
    , haddockClsInstIsOrphan  :: Bool
    , haddockClsInstHead      :: ([Int], SName, [SimpleType])
    , haddockClsInstSynified  :: InstHead GhcRn
    , haddockClsInstTyNames   :: Set.Set Name
    }

-- | TODO: This instance is not lawful. We leave the 'InstHead' segment of the
-- class instance evaluated only to WHNF. This should probably be fixed.
instance NFData HaddockClsInst where
  rnf (HaddockClsInst h n cn o hd s ns) =
              h
    `deepseq` n
    `deepseq` cn
    `deepseq` o
    `deepseq` hd
    `deepseq` s
    `seq`     ns
    `deepseq` ()

instance NamedThing HaddockClsInst where
  getName = haddockClsInstClsName

-- | Stable name for stable comparisons. GHC's `Name` uses unstable
-- ordering based on their `Unique`'s.
newtype SName = SName Name
  deriving newtype NFData

instance Eq SName where
  SName n1 == SName n2 = n1 `stableNameCmp` n2 == EQ

instance Ord SName where
  SName n1 `compare` SName n2 = n1 `stableNameCmp` n2

-- | Simplified type for sorting types, ignoring qualification (not visible
-- in Haddock output) and unifying special tycons with normal ones.
-- For the benefit of the user (looks nice and predictable) and the
-- tests (which prefer output to be deterministic).
data SimpleType = SimpleType SName [SimpleType]
                | SimpleIntTyLit Integer
                | SimpleStringTyLit String
                | SimpleCharTyLit Char
                  deriving (Eq,Ord)

instance NFData SimpleType where
  rnf st =
    case st of
      SimpleType sn sts   -> sn `deepseq` sts `deepseq` ()
      SimpleIntTyLit i    -> rnf i
      SimpleStringTyLit s -> rnf s
      SimpleCharTyLit c   -> rnf c

-- | The three types of instances
data InstType name
  = ClassInst
      { clsiCtx :: [HsType name]
      , clsiTyVars :: LHsQTyVars name
      , clsiSigs :: [Sig name]
      , clsiAssocTys :: [PseudoFamilyDecl name]
      }
  | TypeInst  (Maybe (HsType name)) -- ^ Body (right-hand side)
  | DataInst (TyClDecl name)        -- ^ Data constructors

instance (OutputableBndrId p)
         => Outputable (InstType (GhcPass p)) where
  ppr (ClassInst { .. }) = text "ClassInst"
      <+> ppr clsiCtx
      <+> ppr clsiTyVars
      <+> ppr clsiSigs
  ppr (TypeInst  a) = text "TypeInst"  <+> ppr a
  ppr (DataInst  a) = text "DataInst"  <+> ppr a


-- | Almost the same as 'FamilyDecl' except for type binders.
--
-- In order to perform type specialization for class instances, we need to
-- substitute class variables to appropriate type. However, type variables in
-- associated type are specified using 'LHsTyVarBndrs' instead of 'HsType'.
-- This makes type substitution impossible and to overcome this issue,
-- 'PseudoFamilyDecl' type is introduced.
data PseudoFamilyDecl name = PseudoFamilyDecl
    { pfdInfo :: FamilyInfo name
    , pfdLName :: LocatedN (IdP name)
    , pfdTyVars :: [LHsType name]
    , pfdKindSig :: LFamilyResultSig name
    }


mkPseudoFamilyDecl :: FamilyDecl GhcRn -> PseudoFamilyDecl GhcRn
mkPseudoFamilyDecl (FamilyDecl { .. }) = PseudoFamilyDecl
    { pfdInfo = fdInfo
    , pfdLName = fdLName
    , pfdTyVars = [ L loc (mkType bndr) | L loc bndr <- hsq_explicit fdTyVars ]
    , pfdKindSig = fdResultSig
    }
  where
    mkType :: HsTyVarBndr flag GhcRn -> HsType GhcRn
    mkType (KindedTyVar _ _ (L loc name) lkind) =
        HsKindSig noAnn tvar lkind
      where
        tvar = L (na2la loc) (HsTyVar noAnn NotPromoted (L loc name))
    mkType (UserTyVar _ _ name) = HsTyVar noAnn NotPromoted name


-- | An instance head that may have documentation and a source location.
type DocInstance name = (InstHead name, Maybe (MDoc (IdP name)), Located (IdP name), Maybe Module)

-- | The head of an instance. Consists of a class name, a list of type
-- parameters (which may be annotated with kinds), and an instance type
data InstHead name = InstHead
    { ihdClsName :: IdP name
    , ihdTypes :: [HsType name]
    , ihdInstType :: InstType name
    }


-- | An instance origin information.
--
-- This is used primarily in HTML backend to generate unique instance
-- identifiers (for expandable sections).
data InstOrigin name
    = OriginClass name
    | OriginData name
    | OriginFamily name


instance NamedThing name => NamedThing (InstOrigin name) where

    getName (OriginClass name) = getName name
    getName (OriginData name) = getName name
    getName (OriginFamily name) = getName name


-----------------------------------------------------------------------------
-- * Documentation comments
-----------------------------------------------------------------------------


type LDoc id = Located (Doc id)

type Doc id = DocH (Wrap (ModuleName, OccName)) (Wrap id)
type MDoc id = MetaDoc (Wrap (ModuleName, OccName)) (Wrap id)

type DocMarkup id a = DocMarkupH (Wrap (ModuleName, OccName)) id a

instance NFData Meta where
  rnf (Meta v p) = v `deepseq` p `deepseq` ()

instance NFData id => NFData (MDoc id) where
  rnf (MetaDoc m d) = m `deepseq` d `deepseq` ()

instance (NFData a, NFData mod)
         => NFData (DocH mod a) where
  rnf doc = case doc of
    DocEmpty                  -> ()
    DocAppend a b             -> a `deepseq` b `deepseq` ()
    DocString a               -> a `deepseq` ()
    DocParagraph a            -> a `deepseq` ()
    DocIdentifier a           -> a `deepseq` ()
    DocIdentifierUnchecked a  -> a `deepseq` ()
    DocModule a               -> a `deepseq` ()
    DocWarning a              -> a `deepseq` ()
    DocEmphasis a             -> a `deepseq` ()
    DocBold a                 -> a `deepseq` ()
    DocMonospaced a           -> a `deepseq` ()
    DocUnorderedList a        -> a `deepseq` ()
    DocOrderedList a          -> a `deepseq` ()
    DocDefList a              -> a `deepseq` ()
    DocCodeBlock a            -> a `deepseq` ()
    DocHyperlink a            -> a `deepseq` ()
    DocPic a                  -> a `deepseq` ()
    DocMathInline a           -> a `deepseq` ()
    DocMathDisplay a          -> a `deepseq` ()
    DocAName a                -> a `deepseq` ()
    DocProperty a             -> a `deepseq` ()
    DocExamples a             -> a `deepseq` ()
    DocHeader a               -> a `deepseq` ()
    DocTable a                -> a `deepseq` ()

#if !MIN_VERSION_ghc(8,0,2)
-- These were added to GHC itself in 8.0.2
instance NFData Name where rnf x = seq x ()
instance NFData OccName where rnf x = seq x ()
instance NFData ModuleName where rnf x = seq x ()
#endif

instance NFData id => NFData (Header id) where
  rnf (Header a b) = a `deepseq` b `deepseq` ()

instance NFData id => NFData (Hyperlink id) where
  rnf (Hyperlink a b) = a `deepseq` b `deepseq` ()

instance NFData id => NFData (ModLink id) where
  rnf (ModLink a b) = a `deepseq` b `deepseq` ()

instance NFData Picture where
  rnf (Picture a b) = a `deepseq` b `deepseq` ()

instance NFData Example where
  rnf (Example a b) = a `deepseq` b `deepseq` ()

instance NFData id => NFData (Table id) where
    rnf (Table h b) = h `deepseq` b `deepseq` ()

instance NFData id => NFData (TableRow id) where
    rnf (TableRow cs) = cs `deepseq` ()

instance NFData id => NFData (TableCell id) where
    rnf (TableCell i j c) = i `deepseq` j `deepseq` c `deepseq` ()

exampleToString :: Example -> String
exampleToString (Example expression result) =
    ">>> " ++ expression ++ "\n" ++  unlines result

instance NFData name => NFData (HaddockModInfo name) where
  rnf (HaddockModInfo{..}) =
              hmi_description
    `deepseq` hmi_copyright
    `deepseq` hmi_license
    `deepseq` hmi_maintainer
    `deepseq` hmi_stability
    `deepseq` hmi_portability
    `deepseq` hmi_safety
    `deepseq` hmi_language
    `deepseq` hmi_extensions
    `deepseq` ()

instance NFData LangExt.Extension

data HaddockModInfo name = HaddockModInfo
  { hmi_description :: Maybe (Doc name)
  , hmi_copyright   :: Maybe String
  , hmi_license     :: Maybe String
  , hmi_maintainer  :: Maybe String
  , hmi_stability   :: Maybe String
  , hmi_portability :: Maybe String
  , hmi_safety      :: Maybe String
  , hmi_language    :: Maybe Language
  , hmi_extensions  :: [LangExt.Extension]
  }

emptyHaddockModInfo :: HaddockModInfo a
emptyHaddockModInfo = HaddockModInfo
  { hmi_description = Nothing
  , hmi_copyright   = Nothing
  , hmi_license     = Nothing
  , hmi_maintainer  = Nothing
  , hmi_stability   = Nothing
  , hmi_portability = Nothing
  , hmi_safety      = Nothing
  , hmi_language    = Nothing
  , hmi_extensions  = []
  }


-----------------------------------------------------------------------------
-- * Options
-----------------------------------------------------------------------------


-- | Source-level options for controlling the documentation.
data DocOption
  = OptHide            -- ^ This module should not appear in the docs.
  | OptPrune
  | OptIgnoreExports   -- ^ Pretend everything is exported.
  | OptNotHome         -- ^ Not the best place to get docs for things
                       -- exported by this module.
  | OptShowExtensions  -- ^ Render enabled extensions for this module.
  | OptPrintRuntimeRep -- ^ Render runtime reps for this module (see
                       -- the GHC @-fprint-explicit-runtime-reps@ flag)
  deriving (Eq, Show)


-- | Option controlling how to qualify names
data QualOption
  = OptNoQual         -- ^ Never qualify any names.
  | OptFullQual       -- ^ Qualify all names fully.
  | OptLocalQual      -- ^ Qualify all imported names fully.
  | OptRelativeQual   -- ^ Like local, but strip module prefix
                      --   from modules in the same hierarchy.

data Qualification
  = NoQual
  | FullQual
  | LocalQual Module
  | RelativeQual Module

makeContentsQual :: QualOption -> Qualification
makeContentsQual qual =
  case qual of
    OptNoQual -> NoQual
    _         -> FullQual

makeModuleQual :: QualOption -> Module -> Qualification
makeModuleQual qual mdl =
  case qual of
    OptLocalQual      -> LocalQual mdl
    OptRelativeQual   -> RelativeQual mdl
    OptFullQual       -> FullQual
    OptNoQual         -> NoQual

-- | Whether to hide empty contexts
-- Since pattern synonyms have two contexts with different semantics, it is
-- important to all of them, even if one of them is empty.
data HideEmptyContexts
  = HideEmptyContexts
  | ShowEmptyToplevelContexts

-- | When to qualify @since@ annotations with their package
data SinceQual
  = Always
  | External -- ^ only qualify when the thing being annotated is from
             -- an external package

-----------------------------------------------------------------------------
-- * Renaming
-----------------------------------------------------------------------------

-- | Renames an identifier.
-- The first input is the identifier as it occurred in the comment
-- The second input is the possible namespaces of the identifier
type Renamer = String -> (NameSpace -> Bool) -> [Name]

-----------------------------------------------------------------------------
-- * Error handling
-----------------------------------------------------------------------------

-- | Haddock's own exception type.
data HaddockException
  = HaddockException String
  | WithContext [String] SomeException
  deriving Typeable

instance Show HaddockException where
  show (HaddockException str) = str
  show (WithContext ctxts se)  = unlines $ ["While " ++ ctxt ++ ":\n" | ctxt <- reverse ctxts] ++ [show se]

throwE :: String -> a
instance Exception HaddockException
throwE str = throw (HaddockException str)

withExceptionContext :: MonadCatch m => String -> m a -> m a
withExceptionContext ctxt =
  handle (\ex ->
      case ex of
        HaddockException _ -> throwM $ WithContext [ctxt] (toException ex)
        WithContext ctxts se -> throwM $ WithContext (ctxt:ctxts) se
          ) .
  handle (throwM . WithContext [ctxt])

-----------------------------------------------------------------------------
-- * Pass sensitive types
-----------------------------------------------------------------------------

type instance XRec DocNameI a = GenLocated (Anno a) a
instance UnXRec DocNameI where
  unXRec = unLoc
instance MapXRec DocNameI where
  mapXRec = fmap
instance WrapXRec DocNameI (HsType DocNameI) where
  wrapXRec = noLocA

type instance Anno DocName                           = SrcSpanAnnN
type instance Anno (HsTyVarBndr flag DocNameI)       = SrcSpanAnnA
type instance Anno [LocatedA (HsType DocNameI)]      = SrcSpanAnnC
type instance Anno (HsType DocNameI)                 = SrcSpanAnnA
type instance Anno (DataFamInstDecl DocNameI)        = SrcSpanAnnA
type instance Anno (DerivStrategy DocNameI)          = SrcAnn NoEpAnns
type instance Anno (FieldOcc DocNameI)               = SrcAnn NoEpAnns
type instance Anno (ConDeclField DocNameI)           = SrcSpan
type instance Anno (Located (ConDeclField DocNameI)) = SrcSpan
type instance Anno [Located (ConDeclField DocNameI)] = SrcSpan
type instance Anno (ConDecl DocNameI)                = SrcSpan
type instance Anno (FunDep DocNameI)                 = SrcSpan
type instance Anno (TyFamInstDecl DocNameI)          = SrcSpanAnnA
type instance Anno [LocatedA (TyFamInstDecl DocNameI)] = SrcSpanAnnL
type instance Anno (FamilyDecl DocNameI)               = SrcSpan
type instance Anno (Sig DocNameI)                      = SrcSpan
type instance Anno (InjectivityAnn DocNameI)           = SrcAnn NoEpAnns
type instance Anno (HsDecl DocNameI)                   = SrcSpanAnnA
type instance Anno (FamilyResultSig DocNameI)          = SrcAnn NoEpAnns
type instance Anno (HsOuterTyVarBndrs Specificity DocNameI) = SrcSpanAnnA
type instance Anno (HsSigType DocNameI)                     = SrcSpanAnnA

type XRecCond a
  = ( XParTy a           ~ EpAnn AnnParen
    , NoGhcTc a ~ a
    , MapXRec a
    , UnXRec a
    , WrapXRec a (HsType a)
    )

type instance XForAllTy        DocNameI = EpAnn [AddEpAnn]
type instance XQualTy          DocNameI = EpAnn [AddEpAnn]
type instance XTyVar           DocNameI = EpAnn [AddEpAnn]
type instance XStarTy          DocNameI = EpAnn [AddEpAnn]
type instance XAppTy           DocNameI = EpAnn [AddEpAnn]
type instance XAppKindTy       DocNameI = EpAnn [AddEpAnn]
type instance XFunTy           DocNameI = EpAnn [AddEpAnn]
type instance XListTy          DocNameI = EpAnn AnnParen
type instance XTupleTy         DocNameI = EpAnn AnnParen
type instance XSumTy           DocNameI = EpAnn AnnParen
type instance XOpTy            DocNameI = EpAnn [AddEpAnn]
type instance XParTy           DocNameI = EpAnn AnnParen
type instance XIParamTy        DocNameI = EpAnn [AddEpAnn]
type instance XKindSig         DocNameI = EpAnn [AddEpAnn]
type instance XSpliceTy        DocNameI = DataConCantHappen
type instance XDocTy           DocNameI = EpAnn [AddEpAnn]
type instance XBangTy          DocNameI = EpAnn [AddEpAnn]
type instance XRecTy           DocNameI = EpAnn [AddEpAnn]
type instance XExplicitListTy  DocNameI = EpAnn [AddEpAnn]
type instance XExplicitTupleTy DocNameI = EpAnn [AddEpAnn]
type instance XTyLit           DocNameI = EpAnn [AddEpAnn]
type instance XWildCardTy      DocNameI = EpAnn [AddEpAnn]
type instance XXType           DocNameI = HsCoreTy

type instance XNumTy           DocNameI = NoExtField
type instance XStrTy           DocNameI = NoExtField
type instance XCharTy          DocNameI = NoExtField
type instance XXTyLit          DocNameI = DataConCantHappen

type instance XHsForAllVis        DocNameI = NoExtField
type instance XHsForAllInvis      DocNameI = NoExtField
type instance XXHsForAllTelescope DocNameI = DataConCantHappen

type instance XUserTyVar    DocNameI = NoExtField
type instance XKindedTyVar  DocNameI = NoExtField
type instance XXTyVarBndr   DocNameI = DataConCantHappen

type instance XCFieldOcc   DocNameI = DocName
type instance XXFieldOcc   DocNameI = NoExtField

type instance XFixitySig   DocNameI = NoExtField
type instance XFixSig      DocNameI = NoExtField
type instance XPatSynSig   DocNameI = NoExtField
type instance XClassOpSig  DocNameI = NoExtField
type instance XTypeSig     DocNameI = NoExtField
type instance XMinimalSig  DocNameI = NoExtField

type instance XForeignExport  DocNameI = NoExtField
type instance XForeignImport  DocNameI = NoExtField

type instance XCImport  DocNameI = NoExtField
type instance XCExport  DocNameI = NoExtField

type instance XXForeignImport DocNameI = DataConCantHappen
type instance XXForeignExport DocNameI = DataConCantHappen

type instance XConDeclGADT    DocNameI = NoExtField
type instance XConDeclH98     DocNameI = NoExtField
type instance XXConDecl       DocNameI = DataConCantHappen

type instance XDerivD     DocNameI = NoExtField
type instance XInstD      DocNameI = NoExtField
type instance XForD       DocNameI = NoExtField
type instance XSigD       DocNameI = NoExtField
type instance XTyClD      DocNameI = NoExtField

type instance XNoSig            DocNameI = NoExtField
type instance XCKindSig         DocNameI = NoExtField
type instance XTyVarSig         DocNameI = NoExtField
type instance XXFamilyResultSig DocNameI = DataConCantHappen

type instance XCFamEqn       DocNameI _ = NoExtField
type instance XXFamEqn       DocNameI _ = DataConCantHappen

type instance XCClsInstDecl DocNameI = NoExtField
type instance XCDerivDecl   DocNameI = NoExtField
type instance XStockStrategy    DocNameI = NoExtField
type instance XAnyClassStrategy DocNameI = NoExtField
type instance XNewtypeStrategy  DocNameI = NoExtField
type instance XViaStrategy  DocNameI = LHsSigType DocNameI
type instance XDataFamInstD DocNameI = NoExtField
type instance XTyFamInstD   DocNameI = NoExtField
type instance XClsInstD     DocNameI = NoExtField
type instance XCHsDataDefn  DocNameI = NoExtField
type instance XCFamilyDecl  DocNameI = NoExtField
type instance XClassDecl    DocNameI = NoExtField
type instance XDataDecl     DocNameI = NoExtField
type instance XSynDecl      DocNameI = NoExtField
type instance XFamDecl      DocNameI = NoExtField
type instance XXFamilyDecl  DocNameI = DataConCantHappen
type instance XXTyClDecl    DocNameI = DataConCantHappen

type instance XHsWC DocNameI _ = NoExtField

type instance XHsOuterExplicit    DocNameI _ = NoExtField
type instance XHsOuterImplicit    DocNameI   = NoExtField
type instance XXHsOuterTyVarBndrs DocNameI   = DataConCantHappen

type instance XHsSig      DocNameI = NoExtField
type instance XXHsSigType DocNameI = DataConCantHappen

type instance XHsQTvs        DocNameI = NoExtField
type instance XConDeclField  DocNameI = NoExtField
type instance XXConDeclField DocNameI = DataConCantHappen

type instance XXPat DocNameI = DataConCantHappen
type instance XXHsBindsLR DocNameI a = DataConCantHappen

type instance XSplicePat DocNameI = DataConCantHappen

type instance XCInjectivityAnn DocNameI = NoExtField

type instance XCFunDep DocNameI = NoExtField

type instance XCTyFamInstDecl DocNameI = NoExtField

-----------------------------------------------------------------------------
-- * NFData instances for GHC types
-----------------------------------------------------------------------------

instance NFData RdrName where
  rnf (Unqual on) = rnf on
  rnf (Qual mn on) = mn `deepseq` on `deepseq` ()
  rnf (Orig m on) = m `deepseq` on `deepseq` ()
  rnf (Exact n) = rnf n

instance NFData SourceText where
  rnf NoSourceText   = ()
  rnf (SourceText s) = rnf s

instance NFData FixityDirection where
  rnf InfixL = ()
  rnf InfixR = ()
  rnf InfixN = ()

instance NFData Fixity where
  rnf (Fixity sourceText n dir) =
    sourceText `deepseq` n `deepseq` dir `deepseq` ()

instance NFData ann => NFData (SrcSpanAnn' ann) where
  rnf (SrcSpanAnn a ss) = a `deepseq` ss `deepseq` ()

instance NFData (EpAnn NameAnn) where
  rnf EpAnnNotUsed = ()
  rnf (EpAnn en ann cs) = en `deepseq` ann `deepseq` cs `deepseq` ()

instance NFData NameAnn where
  rnf (NameAnn a b c d e) =
              a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` e
    `deepseq` ()
  rnf (NameAnnCommas a b c d e) =
              a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` e
    `deepseq` ()
  rnf (NameAnnBars a b c d e) =
              a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` e
    `deepseq` ()
  rnf (NameAnnOnly a b c d) =
              a
    `deepseq` b
    `deepseq` c
    `deepseq` d
    `deepseq` ()
  rnf (NameAnnRArrow a b) =
              a
    `deepseq` b
    `deepseq` ()
  rnf (NameAnnQuote a b c) =
              a
    `deepseq` b
    `deepseq` c
    `deepseq` ()
  rnf (NameAnnTrailing a) = rnf a

instance NFData TrailingAnn where
  rnf (AddSemiAnn epaL)  = rnf epaL
  rnf (AddCommaAnn epaL) = rnf epaL
  rnf (AddVbarAnn epaL)  = rnf epaL

instance NFData NameAdornment where
  rnf NameParens     = ()
  rnf NameParensHash = ()
  rnf NameBackquotes = ()
  rnf NameSquare     = ()

instance NFData EpaLocation where
  rnf (EpaSpan ss bs)  = ss `seq` bs `deepseq` ()
  rnf (EpaDelta dp lc) = dp `seq` lc `deepseq` ()

instance NFData EpAnnComments where
  rnf (EpaComments cs) = rnf cs
  rnf (EpaCommentsBalanced cs1 cs2) = cs1 `deepseq` cs2 `deepseq` ()

instance NFData EpaComment where
  rnf (EpaComment t rss) = t `deepseq` rss `seq` ()

instance NFData EpaCommentTok where
  rnf (EpaDocComment ds)  = rnf ds
  rnf (EpaDocOptions s)   = rnf s
  rnf (EpaLineComment s)  = rnf s
  rnf (EpaBlockComment s) = rnf s
  rnf EpaEofComment       = ()


instance NFData a => NFData (Strict.Maybe a) where
  rnf Strict.Nothing  = ()
  rnf (Strict.Just x) = rnf x

instance NFData BufSpan where
  rnf (BufSpan p1 p2) = p1 `deepseq` p2 `deepseq` ()

instance NFData BufPos where
  rnf (BufPos n) = rnf n

instance NFData Anchor where
  rnf (Anchor ss op) = ss `seq` op `deepseq` ()

instance NFData AnchorOperation where
  rnf UnchangedAnchor  = ()
  rnf (MovedAnchor dp) = rnf dp

instance NFData DeltaPos where
  rnf (SameLine n)        = rnf n
  rnf (DifferentLine n m) = n `deepseq` m `deepseq` ()

