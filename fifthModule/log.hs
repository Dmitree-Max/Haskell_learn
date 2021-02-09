module Demo where



data Log a = Log [String] a
        deriving Show


toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = (Log [msg]) . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = case f x of 
        (Log l1 v) -> case g v of
                (Log l2 res) -> Log (l1 ++ l2) res
