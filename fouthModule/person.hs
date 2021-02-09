module Demo where


data Person = Person { firstName :: String, lastName :: String, age :: Int }
        deriving (Show)


abbrFirstName :: Person -> Person
abbrFirstName p@(Person {firstName = n}) = if length n > 1 then p {firstName = head n : "." } else p
