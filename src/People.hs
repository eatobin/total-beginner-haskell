module People where

data Person = Person { name :: String
                     , maxBooks :: Int
                     } deriving Show

addPerson :: [Person] -> Person -> [Person]
addPerson people person = people ++ [person]
