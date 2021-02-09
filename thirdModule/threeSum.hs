module Demo where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 a b c = ((takeFirst a) + (takeFirst b) + (takeFirst c)) : sum3 (safeTail a) (safeTail b) (safeTail c)


takeFirst (x : xs) = x
takeFirst _ = 0


safeTail (x : xs) = xs
safeTail [] = []

