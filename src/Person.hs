{-# LANGUAGE NamedFieldPuns #-}

module Person where

type Name = String
type MaxBooks = Int

data Person = Person { name :: Name
                     , maxBooks :: MaxBooks
                     } deriving (Show, Eq)

getName :: Person -> Name
getName Person {name} = name

setName :: Name -> Person -> Person
setName n p@Person {name} =
  p {name = n}

getMaxBooks :: Person -> MaxBooks
getMaxBooks Person {maxBooks} = maxBooks

setMaxBooks :: MaxBooks -> Person -> Person
setMaxBooks m p@Person {maxBooks} =
  p {maxBooks = m}

personToString :: Person -> String
personToString p = getName p ++ " (" ++ show (getMaxBooks p) ++ " books)"
