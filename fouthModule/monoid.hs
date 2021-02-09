module Demo where


newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' $ Just (mempty)

   -- mappend (Maybe' (Just (mempty))) x       = x
   -- mappend x (Maybe' (Just (mempty :: a)))  = undefined

    mappend (Maybe' Nothing) x = (Maybe' Nothing)
    mappend x (Maybe' Nothing) = (Maybe' Nothing)

    Maybe' (Just x) `mappend` Maybe' (Just y) = Maybe' $ Just (x `mappend` y)
