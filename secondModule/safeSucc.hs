module Demo where



class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x = if x == maxBound then minBound else succ x

  spred :: a -> a
  spred x = if x == minBound then maxBound else pred x


instance SafeEnum Bool where

