module Haddock.Backends.Hyperlinker.Types where


import qualified GHC

import Data.Map (Map)

data Token = Token
    { tkType :: TokenType
    , tkValue :: String
    , tkSpan :: {-# UNPACK #-} !Span
    }
    deriving (Show)

type Position = GHC.RealSrcLoc
type Span = GHC.RealSrcSpan

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

-- | Path for making cross-package hyperlinks in generated sources.
--
-- Used in 'SrcMap' to determine whether module originates in current package
-- or in an external package.
data SrcPath
    = SrcExternal FilePath
    | SrcLocal

-- | Mapping from modules to cross-package source paths.
type SrcMap = Map GHC.Module SrcPath

