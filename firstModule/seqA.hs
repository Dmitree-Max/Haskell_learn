module Data where

seqA :: Integer -> Integer
seqA n      | n == 0     = 1
            | n == 1     = 2
            | n == 2     = 3
            | otherwise  = let
                helper prevprevprev prevprev prev n 
                        | n == 0        = prev
                        | otherwise     = helper prevprev prev (prev + prevprev - 2 * prevprevprev) (n - 1)
                in helper 1 2 3 (n - 2) 
