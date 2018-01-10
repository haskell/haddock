{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Documentation.Markdown.Block (Block(..), toBlocks', toBlockLines, ListType(..), markdown) where

import Prelude
import Control.Monad (msum)
import Data.Char (isDigit,isSpace)
import Documentation.Markdown.Types
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.List (stripPrefix, dropWhileEnd, intercalate)
import Documentation.Haddock.Types
import Documentation.Markdown.Inline
import qualified Data.ByteString.Char8 as BS
import Documentation.Haddock.Doc (docConcat)

markdown :: String -> DocH a b
markdown = docConcat . map blockToDoc . processBlocks . toBlocks'
  where
  processBlocks :: [Block String] -> [Block (DocH a b)]
  processBlocks = map (fmap (docConcat . map inlineToDoc))
                . map (fmap (concatMap (toInline Map.empty . BS.pack)))
                . map toBlockLines


            --    [Block [String]]   Block [Inline]

toBlockLines :: Block String -> Block [String]
toBlockLines = fmap $ map stripEnd . go "" 
  where
    go acc (' ':' ':'\r':'\n':rest) = reverse (dropWhile isSpace acc) : go "" rest
    go acc (' ':' ':     '\n':rest) = reverse (dropWhile isSpace acc) : go "" rest
    go acc (c                :rest) = go (c : acc) rest
    go acc ""                       = [reverse (dropWhile isSpace acc)]


toBlocks' :: String -> [Block String]
toBlocks' = toBlocksLines' . map (go 0) . lines
  where
    go _ "" = ""
    go i ('\r':cs) = go i cs
    go i ('\t':cs) = (replicate j ' ') ++ go (i + j) cs
      where j = 4 - (i `mod` 4)
    go i (c:cs) = c : go (i + 1) cs


toBlocksLines' :: [String]            -- ^ input lines
               -> [Block String]
toBlocksLines' = tightenLists' . start' 

tightenLists' :: [Either Blank (Block String)] -> [Block String]
tightenLists' = go Nothing []
  where
    go _          acc [] = reverse acc
    go mTightList acc (Left Blank  : rest) = go mTightList acc rest
    go mTightList acc (Right (BlockList ltNew contents) : rest) =
        case mTightList of
          Just (ltOld, isTight) | ltOld == ltNew ->
            let acc' = (BlockList ltNew $ (if isTight then tighten else untighten) contents) : acc
            in go mTightList acc' rest
          _ ->
            let (isTight,rest') = checkTight ltNew False rest
                acc' = (BlockList ltNew $ (if isTight then tighten else untighten) contents) : acc
            in go (Just (ltNew, isTight)) acc' rest'
    go _ acc (Right b : rest) = go Nothing (b : acc) rest

      
      
    checkTight _  sawBlank [] = (not sawBlank, [])
    checkTight lt _        (Left Blank : rest) = checkTight lt True rest
    checkTight lt sawBlank rest@(Right (BlockList ltNext _) : _) | ltNext == lt = (not sawBlank, rest)
    checkTight _  _        rest = (False, rest)
     
    tighten (Right [BlockPara t]) = Left t
    tighten (Right []) = Left ""
    tighten x = x

    untighten (Left t) = Right [BlockPara t]
    untighten x = x

    
start' :: [String] -> [Either Blank (Block String)]
start' [] = []
start' (t : ts) = case lineType t of
  
  -- Single line constructs
  LineBlank -> Left Blank : start' ts
  LineHeading level t' -> Right (BlockHeading level t') : start' ts
  LineRule -> Right BlockRule : start' ts
  LineReference x y -> Right (BlockReference x y) : start' ts

  -- Simple span constructs
  LineCode t' ->
    let (ls, ts') = spanIndented 4 ts
    in Right (BlockCode Nothing (intercalate "\n" (t' : ls))) : start' ts'
  LineFenced n desc -> do
    let (ls, ts') = spanFence n
    in Right (BlockCode (Just desc) (intercalate "\n" (t' : ls))) : start' ts'
  LineBlockQuote t' ->
    let (ls, ts') = spanQuotes ts
        blocks = toBlocksLines' (t' : ls)
    in Right (BlockQuote blocks) : start' ts'


  LineText t' ->
    -- Check for underline headings
    let getUnderline :: String -> Maybe Int
        getUnderline s
            | length s < 2 = Nothing
            | all (== '=') s = Just 1
            | all (== '-') s = Just 2
            | otherwise = Nothing
    
    in case listToMaybe ts >>= getUnderline of
         Just level -> Right (BlockHeading level t') : start' (tail ts)
         Nothing -> let listStartIndent x = case listStart x of
                                              Just (_, y) -> take 2 y == "  "
                                              Nothing -> False
                        
                        isNonPara LineBlank = True
                       -- isNonPara LineFenced{} = True
                        isNonPara _ = False
                
                        (ls, ts') = span (\x -> isNonPara (lineType x) || listStartIndent x) ts
                    in Right (BlockPara $ intercalate "\n" $ t' : ls) : start' ts'
 
  LineList ltype t' -> case fmap lineType (listToMaybe ts) of
    
    -- If the next line is a non-indented text line, then we have a lazy list.
    Just (LineText t2') | null (takeWhile (== ' ') t2') -> 
      let (ls', ts') = span (\l -> case lineType l of LineText{} -> True; _ -> False) ts
          ls = dropWhile (== ' ') t' : [ l | LineText l <- map lineType ls' ]
      in Right (BlockList ltype (Right [BlockPara (intercalate "\n" ls)])) : start' ts'
    
    -- If the next line is an indented list, then we have a sublist. I disagree
    -- with this interpretation of Markdown, but it's the way that Github
    -- implements things, so we will too.
    _  | Just t2' <- listToMaybe ts
       , Just t2'' <- stripPrefix "    " t2'
       , LineList{} <- lineType t2'' ->
          let (ls, ts') = spanIndented 4 (drop 1 ts)
              blocks = case strip t' of
                         t'' | null t'' -> toBlocksLines' ls
                             | otherwise -> BlockPlainText t'' : toBlocksLines' ls
          in Right (BlockList ltype (Right blocks)) : start' ts'

    -- Otherwise we have a regular indented list
    _  -> let t'' = dropWhile (== ' ') t'
              n = length t - length t''
              (ls,ts') = spanIndented n ts
              blocks = toBlocksLines' (t'' : ls)
          in Right (BlockList ltype (Right blocks)) : start' ts'


data Blank = Blank

data LineType = LineList ListType String
              | LineCode String
              | LineFence !Int {- fence length -} String {- description -}
              | LineBlockQuote String
              | LineHeading Int String
              | LineBlank
              | LineText String
              | LineRule
              | LineReference String String -- ^ name, destination


-- | Identify what sort of block-level construct a line could be starting
lineType :: String -> LineType
lineType t
    | null $ strip t = LineBlank
    | Just t' <- stripPrefix "```" = let (f, desc) = span (== '`') t' in LineFence (length f + 3) (strip desc)
    | Just t' <- stripPrefix "> " t = LineBlockQuote t'
    | Just (level, t') <- stripHeading t = LineHeading level t'
    | Just t' <- stripPrefix "    " t = LineCode t'
    | isRule t = LineRule
    | Just (ltype, t') <- listStart t = LineList ltype t'
    | Just (name, dest) <- getReference t = LineReference name dest
    | otherwise = LineText t
  where

    isRule :: String -> Bool
    isRule =
        go . strip
      where
        go "* * *" = True
        go "***" = True
        go "*****" = True
        go "- - -" = True
        go "---" = True
        go "___" = True
        go "_ _ _" = True
        go t' = length (takeWhile (== '-') t') >= 5

    stripHeading :: String -> Maybe (Int, String)
    stripHeading t'
        | null x = Nothing
        | otherwise = Just (length x, strip $ dropWhileEnd (== '#') y)
      where
        (x, y) = span (== '#') t'

    getReference :: String -> Maybe (String, String)
    getReference a = do
        b <- stripPrefix "[" $ dropWhile (== ' ') a
        let (name, c) = break (== ']') b
        d <- stripPrefix "]:" c
        Just (name, strip d)


-- | Try to compute the start of a list
listStart :: String -> Maybe (ListType, String)
listStart t0
    -- unordered list
    | Just t' <- stripPrefixChoice ["* ", "*\t", "+ ", "+\t", "- ", "-\t"] t
    = Just (Unordered, t')
    
    -- ordered list
    | Just t' <- stripNumber t
    , Just t'' <- stripPrefixChoice [". ", ".\t", ") ", ")\t"] t'
    = Just (Ordered, t'')
    
    | otherwise = Nothing
  where
    t = stripStart t0

    stripNumber :: String -> Maybe String
    stripNumber x
        | null y = Nothing
        | otherwise = Just z
      where
        (y, z) = span isDigit x


-- | Attempt to strip each of the prefixes in @xs@ from the start of @x@. As
-- soon as one matches, return the remainder of @x@. Prefixes are tried in
-- order. If none match, return @Nothing@.
stripPrefixChoice :: [String] -> String -> Maybe String
stripPrefixChoice xs x = msum $ map (flip stripPrefix x) xs


-- | Split a list of lines, the first of which are part of an indented block,
-- into indented lines and the remaining lines. Also dedent the first of these.
spanIndented :: Int                 -- ^ indent level
             -> [String]            -- ^ input lines
             -> ([String],[String]) -- ^ (newly unindented lines, rest)
spanIndented n = go [] []
  where
  go :: [String]  -- ^ accumulated blank lines
     -> [String]  -- ^ accumulated newly unindented lines
     -> [String]  -- ^ input
     -> ([String], [String])
  go blanks acc [] = (reverse acc, reverse blanks)
  go blanks acc (t:rest)
    | null (strip t) = go (drop n t : blanks) acc rest                    -- ^ a line with only space
    | length x == n && null (strip x) = go [] ([y] ++ blanks ++ acc) rest -- ^ an indented line
    | otherwise = (reverse acc, t:rest)                                         -- ^ an unindented line
    where
      (x,y) = splitAt n t


-- | Split a list of lines, the first of which are part of a blockquote, into
-- the quoted lines and the remaining lines. Also unquote the first of these.
spanQuotes :: [String]            -- ^ lines starting in a blockquote
           -> ([String],[String]) -- ^ (edited blockquote lines, remaining lines)
spanQuotes = go []
  where
  go :: [String] -> [String] -> ([String], [String])
  go acc []           = (reverse acc, [])
  go acc (""  : rest) = (reverse acc, rest)
  go acc (">" : rest) = go ("" : acc) rest
  go acc (t   : rest)
    | Just t' <- stripPrefix "> " t = go (t' : acc) rest
    | otherwise = go (t : acc) rest


spanFence :: Int       -- ^ minimum close fence length
          -> [String]  -- ^ input
          -> ([String], [String])
spanFence n = go []
  where
  go :: [String] -> [String] -> ([String], [String])
  go acc [] = (reverse acc, [])
  go acc (t : rest)
    | all (== '`') t, length t > n = (reverse acc, rest)
    | otherwise = go (t : acc) rest
