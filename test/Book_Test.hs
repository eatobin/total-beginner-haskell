module Book_Test where

import Book
import Person
import Test.HUnit

testMakeBook1 = (~=?)
  Book { title = "Great Expectations", author = "unknown author", borrower = Nothing }
  (makeBook "Great Expectations" "unknown author" Nothing)

testMakeBook2 = (~=?)
  Book { title = "Great Expectations", author = "unknown author", borrower = Just Person { name = "Elvis", maxBooks = 22 } }
  (makeBook "Great Expectations" "unknown author" (Just(Person "Elvis" 22)))
--testGetName = (~=?)
--  "Jack"
--  (getName (Person "Jack" 7))
--
--testSetName = (~=?)
--  Person {name = "Sam", maxBooks = 7}
--  (setName "Sam" (Person "Jack" 7))
--
--testGetMaxBooks = (~=?)
--  77
--  (getMaxBooks (Person "Me" 77))
--
--testSetMaxBooks = (~=?)
--  Person {name = "Sam", maxBooks = 7}
--  (setMaxBooks 7 (Person "Sam" 77))
--
--testPersonToString = (~=?)
--  "Tester (99 books)"
--  (personToString (Person "Tester" 99))
--
runBookTests = runTestTT $ TestList [ testMakeBook1, testMakeBook2 ]
