{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      :  Haddock.Parser
-- Copyright   :  (c) Mateusz Kowalczyk 2013,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable

module Haddock.Parser ( parseParas
                      , parseString
                      , parseIdent
                      ) where

import qualified Documentation.Haddock.Parser as P
import Documentation.Haddock.Types
import Haddock.Types

import DynFlags     ( DynFlags )
import FastString   ( fsLit )
import Lexer        ( mkPState, unP, ParseResult(..) )
import OccName      ( occNameString )
import Parser       ( parseIdentifier )
import RdrName      ( RdrName(Qual) )
import SrcLoc       ( mkRealSrcLoc, GenLocated(..) )
import StringBuffer ( stringToStringBuffer )


parseParas :: DynFlags -> Maybe Package -> String -> MetaDoc mod (Wrap NsRdrName)
parseParas d p = overDoc (P.overIdentifier (parseIdent d)) . P.parseParas p

parseString :: DynFlags -> String -> DocH mod (Wrap NsRdrName)
parseString d = P.overIdentifier (parseIdent d) . P.parseString

parseIdent :: DynFlags -> Namespace -> String -> Maybe (Wrap NsRdrName)
parseIdent dflags ns str0 =
  case unP parseIdentifier (pstate str1) of
    POk _ (L _ name)
      -- Guards against things like 'Q.--', 'Q.case', etc.
      -- See https://github.com/haskell/haddock/issues/952 and Trac #14109
      | Qual _ occ <- name
      , PFailed{} <- unP parseIdentifier (pstate (occNameString occ))
      -> Nothing
      | otherwise
      -> Just (wrap (NsRdrName ns name))
    PFailed{} -> Nothing
  where
    realSrcLc = mkRealSrcLoc (fsLit "<unknown file>") 0 0
    pstate str = mkPState dflags (stringToStringBuffer str) realSrcLc
    (wrap,str1) = case str0 of
                    '(' : s@(c : _) | c /= ',', c /= ')'  -- rule out tuple names
                                    -> (Parenthesized, init s)
                    '`' : s@(_ : _) -> (Backticked,    init s)
                    _               -> (Unadorned,     str0)
