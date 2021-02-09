module Demo where
import Data.Ord




data LogLevel = Error | Warning | Info deriving (Ord, Eq)



cmp :: LogLevel -> LogLevel -> Ordering
cmp x y | x == y        = EQ
cmp x y | x > y         = GT
cmp x y | x < y         = LT
