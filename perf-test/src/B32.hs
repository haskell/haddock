module B32 where

import A (foo)

-- | bar32 is an intermediate node
bar32 :: String
bar32 = show foo
