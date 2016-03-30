module Book_Test where

import Book
import Person
import Test.HUnit

bkb1 = Book { title = "Title1"
             , author = "Author1"
             , borrower = Just Person { name = "Borrower1", maxBooks = 1 } }

bkb2 = Book { title = "Title2"
             , author = "Author2"
             , borrower = Nothing }

testMakeBookNothing = (~=?)
  bkb2
  (makeBook "Title2" "Author2" Nothing)

testMakeBookSomeone = (~=?)
  bkb1
  (makeBook "Title1" "Author1" (Just(Person "Borrower1" 1)))

testGetTitle = (~=?)
  "Title1"
  (getTitle bkb1)

testGetAuthor = (~=?)
  "Author2"
  (getAuthor bkb2)

testGetBorrowerNothing = (~=?)
  Nothing
  (getBorrower bkb2)

testGetBorrowerSomeone = (~=?)
  (Just Person { name = "Borrower1", maxBooks = 1 })
  (getBorrower bkb1)

testSetBorrowerSomeone = (~=?)
  (Book {title = "Title2", author = "Author2",
         borrower = Just (Person {name = "BorrowerNew", maxBooks = 111})})
  (setBorrower (Just Person { name = "BorrowerNew", maxBooks = 111 }) bkb2)

testSetBorrowerNothing = (~=?)
  (Book {title = "Title2", author = "Author2", borrower = Nothing})
  (setBorrower Nothing bkb2)

--testSetAuthor = (~=?)
--  Book { title = "Great Expectations"
--             , author = "unknown author"
--             , borrower = Just Person { name = "Elvis", maxBooks = 22 } }
--  (setAuthor Book { title = "Great Expectations"
--             , author = "unknown author"
--             , borrower = Just Person { name = "Elvis", maxBooks = 22 } })
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
bookTests = TestList [ testMakeBookNothing, testMakeBookSomeone
                     , testGetTitle, testGetAuthor
                     , testGetBorrowerNothing
                     , testGetBorrowerSomeone, testSetBorrowerSomeone
                     , testSetBorrowerNothing ]

runBookTests = runTestTT $ TestList [ bookTests ]
