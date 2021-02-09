module Demo where

qsort :: Ord a => [a] -> [a]
qsort []  = []
qsort [x] = [x]
qsort lst = let hd = head lst 
            in 
                  qsortHelper (filter (< hd) lst) (filter (>= hd) (tail lst)) hd where
                        qsortHelper lst1 lst2 headElement 
                                 | length lst1 > length lst2 = merge (qsort lst1) (qsort (headElement : lst2))
                                 | otherwise                 = merge (qsort (lst1 ++ [headElement])) (qsort lst2)


merge :: Ord a => [a] -> [a] -> [a]
merge lst1 lst2 = helper lst1 lst2 [] where
        helper [] [] res                   = res
        helper [] lst2 res                 = res ++ lst2
        helper lst1 [] res                 = res ++ lst1
        helper lst1@(x:xs) lst2@(y:ys) res = if x < y then helper xs lst2 (res ++ [x]) else helper lst1 ys (res ++ [y])


qsort' :: Ord a => [a] -> [a]
qsort' (x:xs) = qsort (filter (<x) xs) ++ (x : qsort (filter (>=x) xs))
qsort' [] = []
