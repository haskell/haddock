module Bug647 where

class Bug647 a where
	f :: String -- ^ Docs for arg1
	  -> Int    -- ^ Docs for arg2
	  -> Int    -- ^ Docs for arg3
	  -> a