{-# LANGUAGE NamedFieldPuns #-}

module People where

type Name = String
type MaxBooks = Int

data Person = Person { name :: Name
                     , maxBooks :: MaxBooks
                     } deriving Show

type People = [Person]

addPerson :: People -> Person -> People
addPerson xs x = xs ++ [x]

getPerson :: Name -> People -> Maybe Person
getPerson key xs = foldr (\Person {name, maxBooks} acc -> if key == name
  then Just Person {name, maxBooks}
  else acc)
  Nothing xs
