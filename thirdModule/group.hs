module Demo where

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems lst = 
        let     a                          = head lst
                helper [] acc res _        = res ++ [acc]
                helper (x:xs) acc res prev = if x == prev 
                        then helper xs (x : acc) res prev else helper xs [x] (res ++ [acc]) x 
        in helper (tail lst) [a] [] a

-- helper lst acc res prev
