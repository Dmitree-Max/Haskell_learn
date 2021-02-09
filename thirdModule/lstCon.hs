module Demo where

nTimes:: a -> Int -> [a]
nTimes value am = helper value am []

helper value am lst | am == 0    = lst
                    | am > 0     =     helper value (am - 1) (value : lst)
                    | otherwise  = undefined
