--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--
-- This file was modified and integrated into GHC by David Waern 2006.
-- Then moved back into Haddock by Isaac Dupree in 2009 :-)
--

{
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Haddock.Lex (
	Token(..),
	LToken,
	tokenise
 ) where

import Lexer hiding (Token)
import Parser ( parseIdentifier )
import StringBuffer
import RdrName
import SrcLoc
import DynFlags

import Data.Char
import Numeric
import System.IO.Unsafe
}

%wrapper "posn"

$ws    = $white # \n
$digit = [0-9]
$hexdigit = [0-9a-fA-F]
$special =  [\"\@]
$alphanum = [A-Za-z0-9]
$ident    = [$alphanum \'\_\.\!\#\$\%\&\*\+\/\<\=\>\?\@\\\\\^\|\-\~\:]

:-

-- beginning of a paragraph
<0,para> {
 $ws* \n		;
 $ws* \>		{ begin birdtrack }
 $ws* \>\>\>            { strtoken TokExamplePrompt `andBegin` exampleexpr }
 $ws* [\*\-]		{ token TokBullet `andBegin` string }
 $ws* \[		{ token TokDefStart `andBegin` def }
 $ws* \( $digit+ \) 	{ token TokNumber `andBegin` string }
 $ws* $digit+ \. 	{ token TokNumber `andBegin` string }
 $ws*			{ begin string }		
}

-- beginning of a line
<line> {
  $ws* \>		{ begin birdtrack }
  $ws* \>\>\>		{ strtoken TokExamplePrompt `andBegin` exampleexpr }
  $ws* \n		{ token TokPara `andBegin` para }
  -- Here, we really want to be able to say
  -- $ws* (\n | <eof>) 	{ token TokPara `andBegin` para}
  -- because otherwise a trailing line of whitespace will result in 
  -- a spurious TokString at the end of a docstring.  We don't have <eof>,
  -- though (NOW I realise what it was for :-).  To get around this, we always
  -- append \n to the end of a docstring.
  () 			{ begin string }
}

<birdtrack> .*	\n?	{ strtokenNL TokBirdTrack `andBegin` line }

<example> {
  $ws*	\n		{ token TokPara `andBegin` para }
  $ws* \>\>\>	        { strtoken TokExamplePrompt `andBegin` exampleexpr }
  ()			{ begin exampleresult }
}

<exampleexpr> .* \n	{ strtokenNL TokExampleExpression `andBegin` example }

<exampleresult> .* \n	{ strtokenNL TokExampleResult `andBegin` example }

<string,def> {
  $special			{ strtoken $ \s -> TokSpecial (head s) }
  \<\< [^\>]* \>\>              { strtoken $ \s -> TokPic (init $ init $ tail $ tail s) }
  \< [^\>]* \>			{ strtoken $ \s -> TokURL (init (tail s)) }
  \# [^\#]* \#			{ strtoken $ \s -> TokAName (init (tail s)) }
  \/ [^\/]* \/                  { strtoken $ \s -> TokEmphasis (init (tail s)) }
  [\'\`] $ident+ [\'\`]		{ ident }
  \\ .				{ strtoken (TokString . tail) }
  "&#" $digit+ \;		{ strtoken $ \s -> TokString [chr (read (init (drop 2 s)))] }
  "&#" [xX] $hexdigit+ \;	{ strtoken $ \s -> case readHex (init (drop 3 s)) of [(n,_)] -> TokString [chr n] }
  -- allow special characters through if they don't fit one of the previous
  -- patterns.
  [\/\'\`\<\#\&\\]			{ strtoken TokString }
  [^ $special \/ \< \# \n \'\` \& \\ \]]* \n { strtokenNL TokString `andBegin` line }
  [^ $special \/ \< \# \n \'\` \& \\ \]]+    { strtoken TokString }
}

<def> {
  \]				{ token TokDefEnd `andBegin` string }
}

-- ']' doesn't have any special meaning outside of the [...] at the beginning
-- of a definition paragraph.
<string> {
  \]				{ strtoken TokString }
}

{
-- | A located token
type LToken = (Token, AlexPosn)

data Token
  = TokPara
  | TokNumber
  | TokBullet
  | TokDefStart
  | TokDefEnd
  | TokSpecial Char
  | TokIdent [RdrName]
  | TokString String
  | TokURL String
  | TokPic String
  | TokEmphasis String
  | TokAName String
  | TokBirdTrack String
  | TokExamplePrompt String
  | TokExampleExpression String
  | TokExampleResult String
--  deriving Show

tokenPos :: LToken -> (Int, Int)
tokenPos t = let AlexPn _ line col = snd t in (line, col)

-- -----------------------------------------------------------------------------
-- Alex support stuff

type StartCode = Int
type Action = AlexPosn -> String -> StartCode -> (StartCode -> [LToken]) -> DynFlags -> [LToken]

tokenise :: DynFlags -> String -> (Int, Int) -> [LToken]
tokenise dflags str (line, col) = let toks = go (posn, '\n', eofHack str) para in {-trace (show toks)-} toks
  where
    posn = AlexPn 0 line col

    go inp@(pos, _, str) sc =
	  case alexScan inp sc of
		AlexEOF -> []
		AlexError _ -> error "lexical error"
		AlexSkip  inp' _       -> go inp' sc
		AlexToken inp'@(pos',_,_) len act -> act pos (take len str) sc (\sc -> go inp' sc) dflags

-- NB. we add a final \n to the string, (see comment in the beginning of line
-- production above).
eofHack str = str++"\n"

andBegin  :: Action -> StartCode -> Action
andBegin act new_sc = \pos str _ cont dflags -> act pos str new_sc cont dflags

token :: Token -> Action
token t = \pos _ sc cont _ -> (t, pos) : cont sc

strtoken, strtokenNL :: (String -> Token) -> Action
strtoken t = \pos str sc cont _ -> (t str, pos) : cont sc
strtokenNL t = \pos str sc cont _ -> (t (filter (/= '\r') str), pos) : cont sc
-- ^ We only want LF line endings in our internal doc string format, so we
-- filter out all CRs.

begin :: StartCode -> Action
begin sc = \_ _ _ cont _ -> cont sc

-- -----------------------------------------------------------------------------
-- Lex a string as a Haskell identifier

ident :: Action
ident pos str sc cont dflags = 
  case strToHsQNames dflags id of
	Just names -> (TokIdent names, pos) : cont sc
	Nothing -> (TokString str, pos) : cont sc
 where id = init (tail str)

strToHsQNames :: DynFlags -> String -> Maybe [RdrName]
strToHsQNames dflags str0 = 
#if MIN_VERSION_ghc(7,1,0)
  let buffer = stringToStringBuffer str0
#else
  let buffer = unsafePerformIO (stringToStringBuffer str0)
#endif
#if MIN_VERSION_ghc(6,13,0)
      pstate = mkPState dflags buffer noSrcLoc
#else
      pstate = mkPState buffer noSrcLoc dflags
#endif
      result = unP parseIdentifier pstate 
  in case result of 
       POk _ name -> Just [unLoc name] 
       _ -> Nothing
}
