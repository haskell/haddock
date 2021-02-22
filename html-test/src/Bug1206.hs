{-# LANGUAGE Haskell2010 #-}
{- | Bug 1206
-}

{-# language FlexibleInstances #-}

module Bug1206 where

-- | A simple identity type
data T a = T a
  deriving Show

-- | '<>' = 'T (a + b)'
--
-- Docs for the @Semigroup@ instance of @(T Int)@
--
-- ==== __Examples__
--
-- >>> T 2 <> T (3 :: Int)
-- T 5
instance {-# overlapping #-} Semigroup (T Int) where
  (<>) (T a) (T b) = T (a + b)

-- | '<>' = 'T (a <> b)'
--
-- Docs for the @Semigroup@ instance of @Semigroup a => T a@
--
-- ==== __Examples__
--
-- >>> T (Product 1) <> T (Product 2)
-- T (Product {getProduct = 2})
instance {-# overlapping #-} Semigroup a => Semigroup (T a) where
  (<>) (T a) (T b) = T (a <> b)

-- | 'mempty' = 'T mempty'
--
-- Docs for the @Monoid@ instance of @Monoid a => T a@
--
-- ==== __Examples__
--
-- >>> mempty :: T String
-- T ""
instance Monoid a => Monoid (T a) where
  mempty = T mempty
