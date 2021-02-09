module Demo where


data Coord a = Coord a a
        deriving (Show)

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord (size * fromIntegral x + size / 2) (size * fromIntegral y + size / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (ceiling $ x / size - 1) (ceiling $ y / size - 1)

