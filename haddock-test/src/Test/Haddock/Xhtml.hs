module Test.Haddock.Xhtml
    ( Xml
    , parseXml, dumpXml
    , stripLinks, stripLinksWhen, stripAnchorsWhen, stripIdsWhen, stripFooter
    ) where

{-
This module used to actually parse the HTML (using the `xml` parsing library)
which made it was possible to do more proper normalization of things like ids or
names.

However, in the interests of being able to run this from within the GHC
testsuite (where non-bootlib dependencies are a liability), this was swapped
out for some simple string manipulation. Since the test cases aren't very
and since the `xhtml` library already handles the pretty-printing aspect,
this would appear to be a reasonable compromise for now.
-}

import Data.List ( stripPrefix, isPrefixOf )
import Data.Char ( isSpace )

-- | Simple wrapper around the pretty-printed HTML source
newtype Xml = Xml { unXml :: String }

-- | Part of parsing involves dropping the @DOCTYPE@ line
parseXml :: String -> Maybe Xml
parseXml = Just . Xml . dropDocTypeLine
  where
  dropDocTypeLine bs
    | "<!DOCTYPE" `isPrefixOf` bs
    = drop 1 (dropWhile (/= '\n') bs)
    | otherwise
    = bs

dumpXml :: Xml -> String
dumpXml = unXml

type Attr = String
type Value = String

-- | Almost all sanitization operations take the form of:
--
--    * match an attribute key
--    * check something about the value
--    * if the check succeeded, replace the value with a dummy value
--
stripAttrValueWhen
  :: Attr             -- ^ attribute key
  -> Value            -- ^ dummy attribute value
  -> (Value -> Bool)  -- ^ determine whether we should modify the attribute
  -> Xml              -- ^ input XML
  -> Xml              -- ^ output XML
stripAttrValueWhen key fallback p (Xml body) = Xml (filterAttrs body)
  where
  keyEq = key ++ "=\""

  filterAttrs "" = ""
  filterAttrs b@(c:cs)
      | Just valRest <- stripPrefix keyEq b
      , Just (val,rest) <- spanToEndOfString valRest
      = if p val
          then keyEq ++ fallback ++ "\"" ++ filterAttrs rest
          else keyEq ++ val      ++ "\"" ++ filterAttrs rest

      | otherwise
      = c : filterAttrs cs

-- | Spans to the next (unescaped) @\"@ character.
--
-- >>> spanToEndOfString "no closing quotation"
-- Nothing
-- >>> spanToEndOfString "foo\" bar \"baz\""
-- Just ("foo", " bar \"baz\"")
-- >>> spanToEndOfString "foo\\\" bar \"baz\""
-- Just ("foo\\\" bar ", "baz\"")
--
spanToEndOfString :: String -> Maybe (String, String)
spanToEndOfString ('"':rest) = Just ("", rest)
spanToEndOfString ('\\':c:rest)
  | Just (str, rest') <- spanToEndOfString rest
  = Just ('\\':c:str, rest')
spanToEndOfString (c:rest)
  | Just (str, rest') <- spanToEndOfString rest
  = Just (c:str, rest')
spanToEndOfString _ = Nothing


-- | Replace hyperlink targets with @\"#\"@ if they match a predicate
stripLinksWhen :: (Value -> Bool) -> Xml -> Xml 
stripLinksWhen = stripAttrValueWhen "href" "#"

-- | Replace all hyperlink targets with @\"#\"@
stripLinks :: Xml -> Xml
stripLinks = stripLinksWhen (const True)

-- | Replace id's with @\"\"@ if they match a predicate
stripIdsWhen :: (Value -> Bool) -> Xml -> Xml 
stripIdsWhen = stripAttrValueWhen "id" ""

-- | Replace names's with @\"\"@ if they match a predicate
stripAnchorsWhen :: (Value -> Bool) -> Xml -> Xml
stripAnchorsWhen = stripAttrValueWhen "name" ""

-- | Remove the @div@ which has @id=\"footer\"@
stripFooter :: Xml -> Xml
stripFooter (Xml body) = Xml (findDiv body)
  where
  findDiv "" = ""
  findDiv b@(c:cs)
      | Just divRest <- stripPrefix "<div id=\"footer\"" b
      , Just rest <- dropToDiv divRest
      = rest 

      | otherwise
      = c : findDiv cs

  dropToDiv "" = Nothing
  dropToDiv b@(_:cs)
      | Just valRest <- stripPrefix "</div" b
      , valRest' <- dropWhile isSpace valRest
      , Just valRest'' <- stripPrefix ">" valRest'
      = Just valRest''

      | otherwise
      = dropToDiv cs

