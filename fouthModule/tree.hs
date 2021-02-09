module Demo where


data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node x y) = max (height x) (height y) + 1

size :: Tree a -> Int
size (Leaf _) = 1
size (Node x y) = (size x) + (size y) + 1



avg :: Tree Int -> Int
avg t =
    let (s,c) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (x, 1)
    go (Node x y) = case ((go x), (go y)) of
        ((summ1, count1), (summ2, count2)) -> (summ1 + summ2, count1 + count2)
