module Demo where

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes lst = concatMap (\x -> [x^2, x^3]) lst
