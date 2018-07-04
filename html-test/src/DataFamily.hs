{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances #-}
module DataFamily where

-- | some data family
data family Foo a
infixl 3 `Foo`

-- | some type family
type family Fam a :: *

-- | one type instance
type instance Fam (Foo Int) = Int

-- | another type instance
type instance Fam (Foo [a]) = a

-- | A newtype instance
newtype instance Foo [a] = Foo_List [a]

-- | A plain ADT instance
data instance Foo Int
  = Foo_Int Int -- ^ Int
  | Foo_Zero    -- ^ Zero
infixr 5 `Foo_Int`

-- | A GADT like instance
data instance Foo (a, b) where
  Foo_Eq :: a         -- ^ lhs
         -> a         -- ^ rhs
         -> Foo (a,a) -- ^ output
  Foo_MaybeEq :: a -> b -> Foo (a,b)
infixl 2 `Foo_Eq`

instance Eq (Foo Int) where
  Foo_Zero == Foo_Zero = True
  Foo_Zero == Foo_Int n = 0 == n
  Foo_Int n == Foo_Zero = n == 0
  Foo_Int n == Foo_Int m = n == m
