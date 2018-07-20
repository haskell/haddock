{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Bug574 where
-- See https://github.com/haskell/haddock/issues/574

-- | Somthing with a spliced type
foo :: Int -> $(let i = [t| Int |] in [t| $i -> $i |])
foo x y = x + y
