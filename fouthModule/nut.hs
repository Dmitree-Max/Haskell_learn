module Demo where


data Nat = Zero | Suc Nat
        deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1


toNat :: Integer -> Nat
toNat 0 = Zero
toNat x | x < 0 = undefined
        | x > 0 = Suc (toNat (x - 1))


add :: Nat -> Nat -> Nat
add x y = toNat (fromNat x + fromNat y)

mul :: Nat -> Nat -> Nat
mul x y = toNat (fromNat x * fromNat y)

fac :: Nat -> Nat
fac x = toNat (fact $ fromNat x)


fact :: Integer -> Integer
fact 1 = 1
fact 0 = 1
fact x = x * fact (x - 1)
