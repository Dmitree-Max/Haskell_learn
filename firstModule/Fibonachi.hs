module Data where

fibonacci :: Integer -> Integer
fibonacci n | n == 0     = 0
            | n == 1     = 1
            | n == (-1)  = 1
            | n == (-2)  = (-1)
            | n > 0      = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0      = fibonacci (n + 2) - fibonacci (n + 1)


fibonaccif :: Integer -> Integer

fibonachif 0 = 0
fibonaccif n | n > 0     = helper 0 1 (n - 1)
             | n < 0     = helper 0 1 (n + 1)


helper prevprev prev x      | x == 0                 = prev
                            | x > 0                  = helper prev (prevprev + prev) (x - 1)
                            | x < 0                  = helper prev (prevprev - prev) (x + 1)
                            | otherwise              = undefined


