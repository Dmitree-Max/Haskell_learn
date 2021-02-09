module Demo where

import Text.Read hiding (Number)


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
    deriving (Eq, Show)



asToken :: String -> Maybe Token
asToken s = case Text.Read.readMaybe s :: Maybe Int of
        (Just n)  -> Just (Number n)
        (Nothing) -> if s == "+" then Just Plus
                else if s == "-" then Just Minus
                        else if s == "(" then Just LeftBrace
                                else if s == ")" then Just RightBrace else Nothing      
            
            



tokenize :: String -> Maybe [Token]
tokenize input = let w = words input in
         helper w [] 
         where
                helper []     _ = Just []
                helper (x:xs) y = helper xs y >>= insIntoMaybeList token 
                        where token = asToken x


insIntoMaybeList :: Maybe k -> [k] -> Maybe [k]
insIntoMaybeList (Just v) xs = Just (v:xs)
insIntoMaybeList Nothing xs = Nothing


