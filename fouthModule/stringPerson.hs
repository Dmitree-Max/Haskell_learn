module Demo where

import Data.Char

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
        deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int }
        deriving Show

parsePerson :: String -> Either Error Person
parsePerson arg = let firstParse = parseByn arg '\n' in 
        if length firstParse < 3 
        then Left IncompleteDataError
        else
                let (name, nameValue)       = partByEq $ head firstParse
                    (surname, surnameValue) = partByEq $ head $ tail firstParse
                    (ageage, ageValue)         = partByEq $ head $ tail $ tail firstParse in
                        if name /= "firstName" || surname /= "lastName" || ageage /= "age" then Left IncompleteDataError
                        else if null nameValue || null surnameValue || null ageValue then Left ParsingError
                                else if any isDigit ageValue
                                        then Right $ Person {firstName = nameValue, lastName = surnameValue,
                                                 age = read ageValue}
                                        else Left $ IncorrectDataError ageValue
        



parseByn :: String -> Char -> [String]
parseByn arg sep = helper "" arg sep where
        helper tmp [] separ         = [tmp]
        helper tmp (x:xs) separ  | x == separ = tmp : helper "" xs separ
                                 | otherwise  = helper (tmp ++ [x]) xs separ


partByEq :: String -> (String, String)
partByEq arg = case parseByn arg '=' of
        xs -> if length xs /= 2 then ("","")
                else let name  = head xs
                         value = head $ tail xs in
                           if length name < 1 || length value < 1 then ("","")
                           else if last name /= ' ' || head value /= ' ' then ("","")
                                else (init name, tail value)













   

