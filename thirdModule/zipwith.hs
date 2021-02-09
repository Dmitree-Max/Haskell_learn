module Demo where

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 maxof3


maxof3 :: Ord a => a -> a -> a -> a
maxof3 a b c = max (max a b) c

