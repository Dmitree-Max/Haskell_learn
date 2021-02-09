module Demo where

import Control.Monad.State


data Tree a = Leaf a | Fork (Tree a) a (Tree a)
        deriving Show


numberTree :: Tree () -> Tree Integer
numberTree tree =  (evalState numberTreeFromNumber) (tree, 1)



numberTreeFromNumber :: State (Tree (), Integer) (Tree Integer)
numberTreeFromNumber = do
        state <- get
        case fst state of
                (Leaf ()) -> do
                        modify (\(a, b) -> (a, b + 1)) 
                        return (Leaf (snd state))

                (Fork (left) val (right)) -> do
                        put (left, snd state) 
                        newLeft <- numberTreeFromNumber
                        newstate <- get
                        put (right, snd newstate) 
                        modify (\(a, b) -> (a, b + 1))
                        newRight <- numberTreeFromNumber 
                        return (Fork newLeft (snd newstate) newRight)

