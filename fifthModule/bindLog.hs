module Demo where

import Control.Monad

data Log a = Log [String] a
        deriving Show

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = (Log [msg]) . f

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log lst v) f = case f v of
    (Log lst2 v2) -> Log (lst ++ lst2) v2

returnLog :: a -> Log a
returnLog = Log []


instance Monad Log where
    return = returnLog
    (>>=) = bindLog



execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList v []     = return v
execLoggersList v (x:xs) = (x v) >>= (\arg -> execLoggersList arg xs)

