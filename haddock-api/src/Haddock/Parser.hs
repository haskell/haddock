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

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import GHC.Driver.Session ( DynFlags )
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Data.FastString   ( fsLit )
import GHC.Parser.Lexer ( initParserState, unP, ParseResult(POk, PFailed) )
import GHC.Parser       ( parseIdentifier )
import GHC.Types.Name.Occurrence ( occNameString )
import GHC.Types.Name.Reader ( RdrName(..) )
import GHC.Types.SrcLoc ( mkRealSrcLoc, GenLocated(..) )
import GHC.Data.StringBuffer ( stringToStringBuffer )


parseParas :: DynFlags -> Maybe Package -> Text -> MetaDoc mod (Wrap NsRdrName)
parseParas d p = overDoc (P.overIdentifier (parseIdent d)) . P.parseParas p

parseString :: DynFlags -> Text -> DocH mod (Wrap NsRdrName)
parseString d = P.overIdentifier (parseIdent d) . P.parseText

parseIdent :: DynFlags -> Namespace -> Text -> Maybe (Wrap NsRdrName)
parseIdent dflags ns str0 =
  case unP parseIdentifier (pstate (Text.unpack str1)) of
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
    pstate str = initParserState (initParserOpts dflags) (stringToStringBuffer str) realSrcLc
    (wrap,str1) = case Text.uncons str0 of
                    Just ('(', s)
                      | Just (c, _) <- Text.uncons s
                      , c /= ','
                      , c /= ')'
                      -> (Parenthesized, Text.init s)
                    Just ('`', s)
                      | Just (_, _) <- Text.unsnoc s
                      -> (Backticked, s)
                    _ -> (Unadorned,     str0)
