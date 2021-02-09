module Demo where

perms :: [a] -> [[a]]
perms []  = [[]]
perms (x:xs) = concatMap (insertAnyPlace x) (perms xs)



insertAnyPlace :: a -> [a] -> [[a]]
insertAnyPlace newEl lst  = helper lst [] newEl where
        helper [] prev newEl = [prev ++ [newEl]]
        helper lst@(x:xs) prev newEl  = [prev ++ newEl : lst] ++ (helper xs (prev ++ [x]) newEl)
