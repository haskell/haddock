-- | This tests the table markup
module Table
  ( tableWithHeader
  , tableWithoutHeader
  ) where

-- | Table with header.
--
-- +------+--------------+------------------------------------------+
-- | code | message      | description                              |
-- +======+==============+==========================================+
-- | 200  |   @OK@       | operation successful                     |
-- +------+--------------+------------------------------------------+
-- | 204  | @No Content@ | operation successful, no body returned   |
-- +------+--------------+------------------------------------------+
tableWithHeader :: a -> a
tableWithHeader a = a

-- | Table without header.
--
-- +------+--------------+------------------------------------------+
-- | 200  |   @OK@       | operation successful                     |
-- +------+--------------+------------------------------------------+
-- | 204  | @No Content@ | operation successful, no body returned   |
-- +------+--------------+------------------------------------------+
-- | 404  | @Not Found@  | resource not found                       |
-- +------+--------------+------------------------------------------+
tableWithoutHeader :: a -> a
tableWithoutHeader a = a
