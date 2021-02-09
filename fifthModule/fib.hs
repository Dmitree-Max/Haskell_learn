module Demo where

import Control.Monad.State


fibStep :: State (Integer, Integer) ()
fibStep = modify (\(a, b) -> (b, a + b))

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ (replicateM n m)


fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)
