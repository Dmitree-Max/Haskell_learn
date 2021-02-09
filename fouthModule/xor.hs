module Demo where


newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor {getXor = False}
    mappend (Xor x) (Xor y) = Xor (x /= y)


