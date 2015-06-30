module Haddock.Backends.Hyperlinker.Parser (parse) where

import Data.Char
import Data.List

data Token = Token
    { tkType :: TokenType
    , tkValue :: String
    , tkSpan :: Span
    }

data Position = Position
    { posRow :: !Int
    , posCol :: !Int
    }

data Span = Span
    { spStart :: Position
    , spEnd :: Position
    }

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
    | TkUnknown

parse :: String -> [Token]
parse = tokenize . tag . chunk

chunk :: String -> [String]
chunk [] = []
chunk str@(c:_)
    | isSpace c = chunk' $ span isSpace str
chunk str
    | "--" `isPrefixOf` str = chunk' $ span (not . (== '\n')) str
    | "{-" `isPrefixOf` str = chunk' $ chunkComment 0 str
    | otherwise = chunk' $ head $ lex str

chunk' :: (String, String) -> [String]
chunk' (c, rest) = c:(chunk rest)

chunkComment :: Int -> String -> (String, String)
chunkComment _ [] = ("", "")
chunkComment depth ('{':'-':str) =
    let (c, rest) = chunkComment (depth + 1) str
    in ("{-" ++ c, rest)
chunkComment depth ('-':'}':str)
    | depth == 1 = ("-}", str)
    | otherwise =
        let (c, rest) = chunkComment (depth - 1) str
        in ("-}" ++ c, rest)
chunkComment depth (e:str) =
    let (c, rest) = chunkComment depth str
    in (e:c, rest)

tag :: [String] -> [(Span, String)]
tag =
    reverse . snd . foldl aux (Position 1 1, [])
  where
    aux (pos, cs) c =
        let pos' = if c == "\n"
                   then pos { posRow = posRow pos + 1, posCol = 1 }
                   else pos { posCol = posCol pos + length c }
        in (pos', (Span pos pos', c):cs)

tokenize :: [(Span, String)] -> [Token]
tokenize =
    map aux
  where
    aux (sp, str) = Token
        { tkType = classify str
        , tkValue = str
        , tkSpan = sp
        }

classify :: String -> TokenType
classify (c:_)
    | isSpace c = TkSpace
    | isDigit c = TkNumber
    | c `elem` special = TkSpecial
    | c == '#' = TkCpp
    | c == '"' = TkString
    | c == '\'' = TkChar
classify str
    | str `elem` keywords = TkKeyword
    | str `elem` glyphs = TkGlyph
    | all (`elem` symbols) str = TkOperator
    | "--" `isPrefixOf` str = TkComment
    | "{-" `isPrefixOf` str = TkComment
    | isIdentifier str = TkIdentifier
    | otherwise = TkUnknown

keywords :: [String]
keywords =
    [ "as"
    , "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "hiding"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "qualified"
    , "then"
    , "type"
    , "where"
    , "forall"
    , "mdo"
    ]

glyphs :: [String]
glyphs =
    [ ".."
    , ":"
    , "::"
    , "="
    , "\\"
    , "|"
    , "<-"
    , "->"
    , "@"
    , "~"
    , "~#"
    , "=>"
    , "-"
    , "!"
    ]

special :: [Char]
special = "()[]{},;`"

-- TODO: Add support for any Unicode symbol or punctuation.
-- source: http://stackoverflow.com/questions/10548170/what-characters-are-permitted-for-haskell-operators
symbols :: [Char]
symbols = "!#$%&*+./<=>?@\\^|-~:"

isIdentifier :: String -> Bool
isIdentifier (c:str)
    | isLetter c = all (\c' -> isAlphaNum c' || c == '\'') str
isIdentifier _ = False
