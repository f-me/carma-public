module Carma.Utils.Numbers
     ( digitsCount
     , digitsCountInLimit
     ) where


-- | Counts how many digits in an integral number.
--
-- Complexity is @O(n)@.
digitsCount :: (Integral a, Integral b) => a -> b
digitsCount = f 1 . abs
  where f i n | n >= 10   = f (succ i) (n `div` 10)
              | otherwise = i


-- | Checks if number has not more digits than specified as limit.
--
-- This could be more efficient than comparing @digitsCount@ because it stops
-- recursion when limit is exceeded.
digitsCountInLimit
  :: (Integral limit, Integral number) => limit -> number -> Bool
digitsCountInLimit l = f 1 . abs
  where f i n | n >= 10   = i < l && f (succ i) (n `div` 10)
              | otherwise = True
