module Haddock.Parser.Util where

import DynFlags (DynFlags)
import FastString (mkFastString)
import Haddock.Types
import Haddock.Parser
import Lexer (mkPState, unP, ParseResult(POk))
import Parser (parseIdentifier)
import RdrName (RdrName)
import SrcLoc (mkRealSrcLoc, unLoc)
import StringBuffer (stringToStringBuffer)

{-# DEPRECATED parseParasMaybe "use `parseParas` instead" #-}
parseParasMaybe :: DynFlags -> String -> Maybe (Doc RdrName)
parseParasMaybe d = Just . overIdentifier (parseIdent d) . parseParas

{-# DEPRECATED parseStringMaybe "use `parseString` instead" #-}
parseStringMaybe :: DynFlags -> String -> Maybe (Doc RdrName)
parseStringMaybe d = Just . overIdentifier (parseIdent d) . parseString

parseIdent :: DynFlags -> String -> Maybe RdrName
parseIdent dflags str0 =
  let buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (mkFastString "<unknown file>") 0 0
      pstate = mkPState dflags buffer realSrcLc
  in case unP parseIdentifier pstate of
    POk _ name -> Just (unLoc name)
    _ -> Nothing
