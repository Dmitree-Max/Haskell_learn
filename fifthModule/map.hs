module Demo where



data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show



instance Functor (Entry k1 k2) where
    fmap g (Entry (k1, k2) v) = Entry (k1, k2) (g v) 

-- (a -> b) -> (Entry k1 k2) a -> (Entry k1 k2) b



instance Functor (Map k1 k2) where
    fmap g (Map [])     = Map []
    fmap g (Map (x:xs)) =  case (fmap g (Map xs)) of
        Map lst -> Map $ (fmap g x) : lst

-- (a -> b) -> Map [Entry k1 k2 a] -> Map [Entry k1 k2 b]
