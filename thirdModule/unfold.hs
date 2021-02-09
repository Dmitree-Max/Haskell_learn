module Demo where
import Data.List

revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr g b 
  where g x = if x < a then Nothing else Just (x, pred x) 

