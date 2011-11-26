module Haddock.Interface.Rn ( rnDoc, rnHaddockModInfo ) where

import Haddock.Types

import RnEnv       ( dataTcOccs )

import RdrName     ( RdrName, gre_name, GlobalRdrEnv, lookupGRE_RdrName )
import Name        ( Name, isTyConName )
import Outputable  ( ppr, showSDoc )

rnHaddockModInfo :: GlobalRdrEnv -> HaddockModInfo RdrName -> HaddockModInfo Name
rnHaddockModInfo gre hmod =
  let desc = hmi_description hmod
  in hmod { hmi_description = fmap (rnDoc gre) desc }

data Id x = Id {unId::x}
instance Monad Id where (Id v)>>=f = f v; return = Id

rnDoc :: GlobalRdrEnv -> Doc RdrName -> Doc Name
rnDoc gre = unId . do_rn
  where
 do_rn doc_to_rn = case doc_to_rn of 
  
  DocEmpty -> return DocEmpty

  DocAppend a b -> do
    a' <- do_rn a 
    b' <- do_rn b
    return (DocAppend a' b')

  DocString str -> return (DocString str)

  DocParagraph doc -> do
    doc' <- do_rn doc
    return (DocParagraph doc')

  DocIdentifier x -> do
    let choices = dataTcOccs x
    let gres = concatMap (\rdrName ->
                 map gre_name (lookupGRE_RdrName rdrName gre)) choices
    return $ case gres of
      []   -> DocMonospaced (DocString (showSDoc $ ppr x))  -- TODO: DocIdentifierRdrName
      [a]  -> DocIdentifier a
      a:b:_ | isTyConName a -> DocIdentifier a | otherwise -> DocIdentifier b
        -- If an id can refer to multiple things, we give precedence to type
        -- constructors.

  DocModule str -> return (DocModule str)

  DocEmphasis doc -> do
    doc' <- do_rn doc
    return (DocEmphasis doc')

  DocMonospaced doc -> do
    doc' <- do_rn doc 
    return (DocMonospaced doc')
 
  DocUnorderedList docs -> do
    docs' <- mapM do_rn docs
    return (DocUnorderedList docs')

  DocOrderedList docs -> do
    docs' <- mapM do_rn docs
    return (DocOrderedList docs')

  DocDefList list -> do
    list' <- mapM (\(a,b) -> do
      a' <- do_rn a
      b' <- do_rn b
      return (a', b')) list
    return (DocDefList list')

  DocCodeBlock doc -> do
    doc' <- do_rn doc
    return (DocCodeBlock doc')

  DocURL str -> return (DocURL str)

  DocPic str -> return (DocPic str)

  DocAName str -> return (DocAName str)

  DocExamples e -> return (DocExamples e)
