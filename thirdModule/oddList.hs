module Demo where

oddsOnly :: Integral a => [a] -> [a]
oddsOnly arg  = 
        let
                helper [] res =  res
                --helper _  res = res
                helper (x : xs) res = if mod x 2 == 0 then helper xs res else helper xs (x : res)     
        in reverse $ helper arg []

