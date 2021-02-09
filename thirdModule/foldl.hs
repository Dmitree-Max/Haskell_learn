module Demo where

import Data.List

evenOnly :: [a] -> [a]


evenOnly = helper
        where   helper (x:xs)  = helper2 xs
                helper []      = [] 
                helper2 (x:xs) = x : helper xs
                helper2 []     = [] 


