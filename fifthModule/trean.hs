module Demo where



pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x | x <= 0 = []
pythagoreanTriple x = do 
        c <- [3..x]
        a <- [1..(c - 1)]
        b <- [(a + 1) .. (c - 1)]
        if a^2 + b^2 == c^2 then [1] else []
        return (a, b, c)
