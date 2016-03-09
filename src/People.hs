{-# LANGUAGE NamedFieldPuns #-}

module People where

import Data.Maybe

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


-- Notes:

x = fromMaybe "Whoops" (Just "Okay")
y = fromMaybe "Whoops" Nothing
z = fromMaybe 77 Nothing
zz = fromMaybe 77 (Just 66)
-- zzz = fromMaybe "Whoops" (Just 66) -Won't compile - mismatched return types.

st = Person "Scott" 8
et = Person "Eric" 7
bt = Person "Brenda" 3
boys = [et,st]
fam = addPerson boys bt
choice = getPerson "Brenda" fam
noOne = getPerson "Jimmy" fam
bren = fromMaybe (Person "No Match" 0) (choice)
bad = fromMaybe (Person "No Match" 0) (noOne)

test = isJust (Just 3)
test2 = if isJust (Just 6) then "Good" else "Bad"
test3 = if isJust choice then "Good" else "Bad"
test4 = if isJust noOne then "Good" else "Bad"
