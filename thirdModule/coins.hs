module Demo where

coins = [7, 12] 

--change :: (Ord a, Num a) => a -> [[a]]
change 0   = [[]]
change arg = [x : ys | x <- coins, x <= arg, ys <- (change (arg - x)), x + sum ys == arg]
