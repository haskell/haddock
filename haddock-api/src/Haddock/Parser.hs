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
import Lexer        ( mkPState, unP, ParseResult(POk) )
import Parser       ( parseIdentifier )
import RdrName      ( RdrName )
import SrcLoc       ( mkRealSrcLoc, unLoc )
import StringBuffer ( stringToStringBuffer )


parseParas :: DynFlags -> Maybe Package -> String -> MetaDoc mod (Wrap RdrName)
parseParas d p = overDoc (P.overIdentifier (parseIdent d)) . P.parseParas p

parseString :: DynFlags -> String -> DocH mod (Wrap RdrName)
parseString d = P.overIdentifier (parseIdent d) . P.parseString

parseIdent :: DynFlags -> String -> Maybe (Wrap RdrName)
parseIdent dflags str0 =
  let buffer = stringToStringBuffer str1
      realSrcLc = mkRealSrcLoc (fsLit "<unknown file>") 0 0
      pstate = mkPState dflags buffer realSrcLc
      (wrap,str1) = case str0 of
                      '(' : s@(_ : _) -> (Parenthesized, init s)
                      '`' : s@(_ : _) -> (Backticked,    init s)
                      _               -> (Unadorned,     str0)
  in case unP parseIdentifier pstate of
    POk _ name -> Just (wrap (unLoc name))
    _ -> Nothing
