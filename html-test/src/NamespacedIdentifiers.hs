module NamespacedIdentifiers where

-- | A link to:
--
--   * the type t'Bar'
--   * the constructor v'Bar'
--
data Foo = Bar

-- | A link to the value v'Foo' (which shouldn't exist).
data Bar
