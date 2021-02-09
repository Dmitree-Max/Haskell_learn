module Data where

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let
                steps = 1000
                stepSize = (b - a)/steps
        in if b > a then helper f (f a) (a + stepSize) stepSize steps 0  
        else if a == b then 0 else -integration f b a where 
                                helper function prevValue argument stepSize iteration sum
                                        | iteration == 0             = sum 
                                        | otherwise                  = let
                                                        newValue = function argument in
                                                helper function newValue (argument + stepSize) stepSize 
                                                (iteration - 1) 
                                                (sum + (newValue + prevValue)/2*stepSize)
