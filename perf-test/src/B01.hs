module B01 where

import A (foo)

-- | bar01 is an intermediate node
bar01 :: String
bar01 = show foo
