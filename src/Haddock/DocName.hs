{-# OPTIONS_HADDOCK hide #-}

--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.DocName where


import GHC
import Name
import Binary


data DocName = Documented Name Module | Undocumented Name
  deriving Eq


-- | The 'OccName' belonging to this name
docNameOcc :: DocName -> OccName
docNameOcc = nameOccName . getName


instance NamedThing DocName where
  getName (Documented name _) = name
  getName (Undocumented name) = name


instance Binary DocName where
  put_ bh (Documented name modu) = do
    putByte bh 0
    put_ bh name
    put_ bh modu
  put_ bh (Undocumented name) = do
    putByte bh 1
    put_ bh name

  get bh = do
    h <- getByte bh
    case h of
      0 -> do
        name <- get bh
        modu <- get bh
        return (Documented name modu)
      1 -> do
        name <- get bh
        return (Undocumented name)
      _ -> error "get DocName: Bad h"
