{-# OPTIONS_HADDOCK ignore-exports #-}

module Bug1531 (exportedFunction) where

-- | This function is exported.
exportedFunction :: a
exportedFunction = undefined

-- | This function isn't, but should still be included due to the presence
--   of the ignore-exports pragma.
internalFunction :: a
internalFunction = undefined
