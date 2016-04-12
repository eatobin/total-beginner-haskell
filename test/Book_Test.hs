module Book_Test where

import Book
import Person
import Person_Test
import Test.HUnit

bk1 = Book { title = "Title1"
             , author = "Author1"
             , borrower = Just p1 }

bk2 = Book { title = "Title2"
             , author = "Author2"
             , borrower = Nothing }

testMakeBookNothing = (~=?)
  bk2
  (makeBook "Title2" "Author2" Nothing)

testMakeBookSomeone = (~=?)
  bk1
  (makeBook "Title1" "Author1" (Just (Person "Person1" 1)))

testGetTitle = (~=?)
  "Title1"
  (getTitle bk1)

testGetAuthor = (~=?)
  "Author2"
  (getAuthor bk2)

testGetBorrowerNothing = (~=?)
  Nothing
  (getBorrower bk2)

testGetBorrowerSomeone = (~=?)
  (Just Person { name = "Person1", maxBooks = 1 })
  (getBorrower bk1)

testSetBorrowerSomeone = (~=?)
  (Book {title = "Title2", author = "Author2",
         borrower = Just (Person {name = "PersonNew", maxBooks = 111})})
  (setBorrower (Just Person { name = "PersonNew", maxBooks = 111 }) bk2)

testSetBorrowerNothing = (~=?)
  (Book {title = "Title2", author = "Author2", borrower = Nothing})
  (setBorrower Nothing bk2)

testBookToStringSomeone = (~=?)
  "Title1 by Author1; Checked out to Person1"
  (bookToString bk1)

testBookToStringNothing = (~=?)
  "Title2 by Author2; Available"
  (bookToString bk2)

bookTests = TestList [ testMakeBookNothing, testMakeBookSomeone
                     , testGetTitle, testGetAuthor
                     , testGetBorrowerNothing
                     , testGetBorrowerSomeone, testSetBorrowerSomeone
                     , testSetBorrowerNothing, testBookToStringSomeone
                     , testBookToStringNothing ]

runBookTests = runTestTT $ TestList [ bookTests ]
