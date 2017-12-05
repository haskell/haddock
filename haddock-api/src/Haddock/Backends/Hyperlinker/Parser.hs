module Haddock.Backends.Hyperlinker.Parser (parse) where


import Data.Char
import Data.List
import Data.Maybe
import qualified Lexer as L
import Lexer (Token(..))
import qualified GHC
import SrcLoc
import FastString
import Data.Either
import StringBuffer

import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Types as T

-- | Turn source code string into a stream of more descriptive tokens.
--
-- Result should retain original file layout (including comments, whitespace,
-- etc.), i.e. the following "law" should hold:
--
-- @concat . map 'tkValue' . 'parse' = id@
parse :: GHC.DynFlags -> FilePath -> String -> [T.Token]
parse dflags fp s = ghcToks (processCPP dflags fp s)

-- | Remove CPP lines and reinsert as comments
processCPP :: GHC.DynFlags -> FilePath -> String -> [(Located L.Token, String)]
processCPP dflags fpath s = addSrc (go start (groupCPP (lines s)))
  where
    start = mkRealSrcLoc (mkFastString fpath) 1 1

    addSrc = GHC.addSourceToTokens start (stringToStringBuffer s)


    go :: GHC.RealSrcLoc -> [Either String String] -> [Located L.Token]
    go _   [] = []
    go pos xs =
      let (cs, cpps, rest) = gather xs
      in case L.lexTokenStream (stringToStringBuffer (unlines cs)) pos dflags of
           L.PFailed ss msg ->
            let (new_loc, token) = mkComment (pos, []) (unlines cs)
            in token ++ go new_loc rest
           L.POk ss toks ->
            let (new_loc, cpp) = foldl' mkComment (L.loc ss, []) cpps
            in toks ++ reverse cpp ++ go new_loc rest

    gather :: [Either String String] -> ([String], [String], [Either String String])
    gather xs =
      let (cs,rest) = span isRight xs
          (cpps, rest') = span isLeft rest
      in ([x | Right x <- cs], [x | Left x <- cpps], rest')

    mkComment (start, cpp) str =
      let end = foldl' advanceSrcLoc start str
          new = advanceSrcLoc end '\n'
      in (new, L (RealSrcSpan $ mkRealSrcSpan start end) (ITlineComment str) : cpp)


-- | Find lines which start with a hash and remove them
groupCPP :: [String] -> [Either String String]
groupCPP [] = []
groupCPP (line@('#':_):lines) = Left line : groupCPP lines
groupCPP (l:ls)                = Right l : groupCPP ls

ghcToks :: [(Located L.Token, String)] -> [T.Token]
ghcToks = reverse . snd . foldl' go (start, [])
  where
    start = mkRealSrcLoc (mkFastString "lexing") 1 1
    go ::  (RealSrcLoc, [T.Token]) -> (Located L.Token, String) -> (RealSrcLoc, [T.Token])
    go (pos, toks) (L l tok, raw) =
        (next, [Token (classify tok) raw rss | RealSrcSpan rss <- [l], not (null raw)]
                ++ maybeToList whitespace
                ++ toks)
        where
          (next, whitespace) = mkWhitespace pos l

-- | Find the correct amount of whitespace between tokens.
mkWhitespace :: RealSrcLoc -> SrcSpan -> (RealSrcLoc, Maybe T.Token)
mkWhitespace prev span
    = case span of
        UnhelpfulSpan _ -> (prev,Nothing)
        RealSrcSpan s   -> (end, Just (Token TkSpace wsstring wsspan))
          where
            start = realSrcSpanStart s
            end = realSrcSpanEnd s
            wsspan = mkRealSrcSpan prev start
            nls = srcLocLine start - srcLocLine prev
            spaces = if nls == 0 then srcLocCol start - srcLocCol prev
                                 else srcLocCol start - 1
            wsstring = replicate nls '\n' ++ replicate spaces ' '

-- | Classify given string as appropriate Haskell token.
classify :: L.Token -> TokenType
classify tok =
  case tok of
    ITas -> TkKeyword
    ITcase -> TkKeyword
    ITclass -> TkKeyword
    ITdata -> TkKeyword
    ITdefault -> TkKeyword
    ITderiving -> TkKeyword
    ITdo -> TkKeyword
    ITelse -> TkKeyword
    IThiding -> TkKeyword
    ITforeign -> TkKeyword
    ITif -> TkKeyword
    ITimport -> TkKeyword
    ITin -> TkKeyword
    ITinfix -> TkKeyword
    ITinfixl -> TkKeyword
    ITinfixr -> TkKeyword
    ITinstance -> TkKeyword
    ITlet -> TkKeyword
    ITmodule -> TkKeyword
    ITnewtype -> TkKeyword
    ITof -> TkKeyword
    ITqualified -> TkKeyword
    ITthen -> TkKeyword
    ITtype -> TkKeyword
    ITwhere -> TkKeyword

    ITforall {} -> TkKeyword
    ITexport -> TkKeyword
    ITlabel -> TkKeyword
    ITdynamic -> TkKeyword
    ITsafe -> TkKeyword
    ITinterruptible -> TkKeyword
    ITunsafe -> TkKeyword
    ITstdcallconv -> TkKeyword
    ITccallconv -> TkKeyword
    ITcapiconv -> TkKeyword
    ITprimcallconv -> TkKeyword
    ITjavascriptcallconv -> TkKeyword
    ITmdo -> TkKeyword
    ITfamily -> TkKeyword
    ITrole -> TkKeyword
    ITgroup -> TkKeyword
    ITby -> TkKeyword
    ITusing -> TkKeyword
    ITpattern -> TkKeyword
    ITstatic -> TkKeyword
    ITstock -> TkKeyword
    ITanyclass -> TkKeyword

    ITunit -> TkKeyword
    ITsignature -> TkKeyword
    ITdependency -> TkKeyword
    ITrequires -> TkKeyword

    ITinline_prag {} -> TkPragma
    ITspec_prag         {}  -> TkPragma
    ITspec_inline_prag  {} -> TkPragma
    ITsource_prag       {} -> TkPragma
    ITrules_prag        {} -> TkPragma
    ITwarning_prag      {} -> TkPragma
    ITdeprecated_prag   {} -> TkPragma
    ITline_prag -> TkPragma
    ITscc_prag          {} -> TkPragma
    ITgenerated_prag    {} -> TkPragma
    ITcore_prag         {} -> TkPragma
    ITunpack_prag       {} -> TkPragma
    ITnounpack_prag     {} -> TkPragma
    ITann_prag          {} -> TkPragma
    ITcomplete_prag     {} -> TkPragma
    ITclose_prag -> TkPragma
    IToptions_prag {} -> TkPragma
    ITinclude_prag {} -> TkPragma
    ITlanguage_prag -> TkPragma
    ITvect_prag         {} -> TkPragma
    ITvect_scalar_prag  {} -> TkPragma
    ITnovect_prag       {} -> TkPragma
    ITminimal_prag      {} -> TkPragma
    IToverlappable_prag {} -> TkPragma
    IToverlapping_prag  {} -> TkPragma
    IToverlaps_prag     {} -> TkPragma
    ITincoherent_prag   {} -> TkPragma
    ITctype             {} -> TkPragma

    ITdotdot -> TkGlyph
    ITcolon -> TkGlyph
    ITdcolon {} -> TkGlyph
    ITequal -> TkGlyph
    ITlam -> TkGlyph
    ITlcase -> TkGlyph
    ITvbar -> TkGlyph
    ITlarrow {} -> TkGlyph
    ITrarrow {} -> TkGlyph
    ITat -> TkGlyph
    ITtilde -> TkGlyph
    ITtildehsh -> TkGlyph
    ITdarrow {} -> TkGlyph
    ITminus -> TkGlyph
    ITbang -> TkGlyph
    ITdot -> TkGlyph
    ITtypeApp -> TkGlyph

    ITbiglam -> TkGlyph

    ITocurly  -> TkSpecial
    ITccurly  -> TkSpecial
    ITvocurly -> TkSpecial
    ITvccurly -> TkSpecial
    ITobrack -> TkSpecial
    ITopabrack  -> TkSpecial
    ITcpabrack  -> TkSpecial
    ITcbrack -> TkSpecial
    IToparen -> TkSpecial
    ITcparen -> TkSpecial
    IToubxparen -> TkSpecial
    ITcubxparen -> TkSpecial
    ITsemi -> TkSpecial
    ITcomma -> TkSpecial
    ITunderscore -> TkSpecial
    ITbackquote -> TkSpecial
    ITsimpleQuote -> TkSpecial

    ITvarid   {} -> TkIdentifier
    ITconid   {} -> TkIdentifier
    ITvarsym  {} -> TkIdentifier
    ITconsym  {} -> TkIdentifier
    ITqvarid  {} -> TkIdentifier
    ITqconid  {} -> TkIdentifier
    ITqvarsym {} -> TkIdentifier
    ITqconsym {} -> TkIdentifier

    ITdupipvarid   {}  -> TkUnknown
    ITlabelvarid   {} -> TkUnknown

    ITchar     {} -> TkChar
    ITstring   {} -> TkString
    ITinteger  {} -> TkNumber
    ITrational {}  -> TkNumber

    ITprimchar {} -> TkChar
    ITprimstring {} -> TkString
    ITprimint    {} -> TkNumber
    ITprimword   {} -> TkUnknown
    ITprimfloat  {} -> TkUnknown
    ITprimdouble {} -> TkUnknown

    ITopenExpQuote {}  -> TkSpecial
    ITopenPatQuote     -> TkSpecial
    ITopenDecQuote   -> TkSpecial
    ITopenTypQuote   -> TkSpecial
    ITcloseQuote {}  -> TkSpecial
    ITopenTExpQuote {} -> TkSpecial
    ITcloseTExpQuote -> TkSpecial
    ITidEscape   {}  -> TkUnknown
    ITparenEscape    -> TkSpecial
    ITidTyEscape   {} -> TkUnknown
    ITparenTyEscape   -> TkSpecial
    ITtyQuote         -> TkSpecial
    ITquasiQuote {}   -> TkUnknown
    -- ITquasiQuote(quoter, quote, loc)
    -- represents a quasi-quote of the form
    -- [quoter| quote |]
    ITqQuasiQuote {} -> TkUnknown
    -- ITqQuasiQuote(Qual, quoter, quote, loc)
    -- represents a qualified quasi-quote of the form
    -- [Qual.quoter| quote |]

    ITproc -> TkKeyword
    ITrec  -> TkKeyword
    IToparenbar {}  -> TkGlyph
    ITcparenbar {}  -> TkGlyph
    ITlarrowtail {} -> TkGlyph
    ITrarrowtail {} -> TkGlyph
    ITLarrowtail {} -> TkGlyph
    ITRarrowtail {} -> TkGlyph

    ITunknown {}     -> TkUnknown
    ITeof            -> TkUnknown

    ITdocCommentNext {} -> TkComment
    ITdocCommentPrev {} -> TkComment
    ITdocCommentNamed {} -> TkComment
    ITdocSection {}     -> TkComment
    ITdocOptions {}     -> TkComment
    ITlineComment {}   -> TkComment
    ITblockComment {}  -> TkComment
