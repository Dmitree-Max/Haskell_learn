module Demo where

import Prelude hiding (lookup)
import qualified Data.List as L



class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)


instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap [])             = Nothing
    lookup k (ListMap ((x, y):xs))    = if k == x then (Just y) else lookup k (ListMap xs)
    insert k v (ListMap lst)          = ListMap ((k,v) : filter (\(x, y) -> x /= k) lst)
    delete k (ListMap lst)            = ListMap (filter (\(x, y) -> x /= k) lst)


