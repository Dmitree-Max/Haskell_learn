module Demo where



data Bit = Zero | One
        deriving Show 
data Sign = Minus | Plus
        deriving Show

data Z = Z Sign [Bit]
        deriving Show


data Zint = Zint Sign Integer
        deriving Show

zToInt :: Z -> Zint
zToInt (Z s []) =  Zint s 0
zToInt (Z s (x:xs)) =  case zToInt (Z s xs) of  
        (Zint s i) -> Zint s (bitToInt x + 2 * i)
                where bitToInt Zero = 0
                      bitToInt One  = 1


zToBit :: Zint -> Z
zToBit (Zint s 0) = Z s []
zToBit (Zint s x) = case zToBit (Zint s (x `div` 2)) of 
        (Z s lst) -> Z s $ intToBit (x `mod` 2) : lst where
                intToBit 0 = Zero
                intToBit 1 = One


zToInteger :: Z -> Integer
zToInteger (Z s ls) = case zToInt (Z s ls) of
        (Zint Plus num)  -> num
        (Zint Minus num) -> -num


integerToZ :: Integer -> Z
integerToZ x | x >= 0    = zToBit (Zint Plus x)
integerToZ x | x < 0     = zToBit (Zint Minus (-x))


add :: Z -> Z -> Z
add x y = integerToZ $ zToInteger x + zToInteger y


mul :: Z -> Z -> Z
mul x y = integerToZ $ zToInteger x * zToInteger y
