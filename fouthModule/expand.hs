module Demo where


infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)


expand :: Expr -> Expr
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = multiplyLists (exprToList $ expand e1) (exprToList $ expand e2)
expand e           = e


multiplyLists :: [Expr] -> [Expr] -> Expr
multiplyLists a b = listToExpr(helper a b []) where
        helper []     y res = res
        helper (x:xs) y res = helper xs y (res ++ map (x :*:) y)


listToExpr :: [Expr] -> Expr
listToExpr [x] = x
listToExpr (x:xs) = x :+: (listToExpr xs) 


exprToList :: Expr -> [Expr]
exprToList (a :+: b) = exprToList a ++ exprToList b
exprToList e         = [e]









