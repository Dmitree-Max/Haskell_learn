module Demo where
import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x then Just x else findDigit xs


findDigitOrX :: [Char] -> Char
findDigitOrX str = case findDigit str of
    Nothing -> 'X'
    Just x  -> x
