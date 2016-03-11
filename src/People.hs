--{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE ViewPatterns #-}

--module People where

--import Data.Maybe

--data Person = Person { name :: String
--                     , maxBooks :: Int
--                     } deriving Show

--type People = [Person]

----addPerson :: People -> Person -> People
----addPerson xs x = xs ++ [x]

----getPerson :: Name -> People -> Maybe Person
----getPerson key xs = foldr (\Person {name, maxBooks} acc -> if key == name
----  then Just Person {name, maxBooks}
----  else acc)
----  Nothing xs


---- getPerson' :: People -> Maybe Person
---- getPerson' [] = Nothing
---- getPerson' (Person {name = "Brenda", maxBooks}:_) = Just Person {name = "Eric", maxBooks}
---- --isClientMaleR IndividualR {person = PersonR {genderR = Male}} = True
---- getPerson' (_:xs) = getPerson' xs

----getPerson' :: Name -> People -> Maybe Person
----getPerson' _ [] = Nothing
----getPerson' v (Person {name = "Brenda", maxBooks}:_) = Just Person {name = v, maxBooks}
------isClientMaleR IndividualR {person = PersonR {genderR = Male}} = True
----getPerson' v (_:xs) = getPerson' v xs

---- tester v (name -> v) = True

---- Notes:

--x = fromMaybe "Whoops" (Just "Okay")
--y = fromMaybe "Whoops" Nothing
--z = fromMaybe 77 Nothing
--zz = fromMaybe 77 (Just 66)
---- zzz = fromMaybe "Whoops" (Just 66) -Won't compile - mismatched return types.

--st = Person "Scott" 8
--et = Person "Eric" 7
--bt = Person "Brenda" 3
--boys = [et,st]
--fam = addPerson boys bt
--choice = getPerson "Brenda" fam
--noOne = getPerson "Jimmy" fam
--bren = fromMaybe (Person "No Match" 0) (choice)
--bad = fromMaybe (Person "No Match" 0) (noOne)

--test = isJust (Just 3)
--test2 = if isJust (Just 6) then "Good" else "Bad"
--test3 = if isJust choice then "Good" else "Bad"
--test4 = if isJust noOne then "Good" else "Bad"

----choice' = getPerson' "Brenda" fam
