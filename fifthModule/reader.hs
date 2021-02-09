module Demo where

import Control.Monad.Reader


type User = String
type Password = String
type UsersTable = [(User, Password)]


usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
        users <- ask
        if null users
        then
            return []
        else do
                user <- asks (head) 
                if snd user == "123456"
                then
                        case runReader usersWithBadPasswords (tail users) of
                                xs ->  return ((fst user) : xs)
                else
                        local tail usersWithBadPasswords
