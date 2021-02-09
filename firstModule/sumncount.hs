module Data where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper (abs x) 0 0 where 
        helper numberBeginning sum digitalsAmount
                | (numberBeginning == 0) && (sum == 0)     = (sum, digitalsAmount + 1)
                | numberBeginning == 0                     = (sum, digitalsAmount)
                | otherwise                                = helper (div numberBeginning 10) 
                (sum + mod numberBeginning 10) (digitalsAmount + 1)
                
