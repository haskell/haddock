module Haddock.Backends.Hyperlinker.Types where


import qualified GHC

import Data.Map (Map)

data Token = Token
    { tkType :: TokenType
    , tkValue :: String
    , tkSpan :: {-# UNPACK #-} !GHC.RealSrcSpan
    }
    deriving (Show)

data Position = Position
    { posRow :: !Int
    , posCol :: !Int
    }
    deriving (Eq, Ord, Show)

data Span = Span
    { spStart :: !Position
    , spEnd   :: !Position
    }
    deriving (Show)

-- | Tests whether the first span "contains" the other span, meaning
-- that it covers at least as much source code. True where spans are equal.
containsSpan :: Span -> Span -> Bool
containsSpan s1 s2 = spStart s1 <= spStart s2 && spEnd s1 >= spEnd s2

data TokenType
    = TkIdentifier
    | TkKeyword
    | TkString
    | TkChar
    | TkNumber
    | TkOperator
    | TkGlyph
    | TkSpecial
    | TkSpace
    | TkComment
    | TkCpp
    | TkPragma
    | TkUnknown
    deriving (Show, Eq)


data RichToken = RichToken
    { rtkToken :: Token
    , rtkDetails :: Maybe TokenDetails
    }

data TokenDetails
    = RtkVar GHC.Name
    | RtkType GHC.Name
    | RtkBind GHC.Name
    | RtkDecl GHC.Name
    | RtkModule GHC.ModuleName
    deriving (Eq)


rtkName :: TokenDetails -> Either GHC.Name GHC.ModuleName
rtkName (RtkVar name) = Left name
rtkName (RtkType name) = Left name
rtkName (RtkBind name) = Left name
rtkName (RtkDecl name) = Left name
rtkName (RtkModule name) = Right name


-- | Path for making cross-package hyperlinks in generated sources.
--
-- Used in 'SrcMap' to determine whether module originates in current package
-- or in an external package.
data SrcPath
    = SrcExternal FilePath
    | SrcLocal

-- | Mapping from modules to cross-package source paths.
type SrcMap = Map GHC.Module SrcPath

mkSrcMap :: Map GHC.Module SrcPath -> SrcMap
mkSrcMap srcs = srcs
