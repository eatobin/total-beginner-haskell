module Book_Test where

import Book
import Borrower
import Borrower_Test
import Test.HUnit

bk1 = Book { title = "Title1"
             , author = "Author1"
             , borrower = Just br1 }

bk2 = Book { title = "Title2"
             , author = "Author2"
             , borrower = Nothing }

testMakeBookNothing = (~=?)
  bk2
  (makeBook "Title2" "Author2" Nothing)

testMakeBookSomeone = (~=?)
  bk1
  (makeBook "Title1" "Author1" (Just (Borrower "Borrower1" 1)))

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
  (Just Borrower { name = "Borrower1", maxBooks = 1 })
  (getBorrower bk1)

testSetBorrowerSomeone = (~=?)
  (Book {title = "Title2", author = "Author2",
         borrower = Just (Borrower {name = "BorrowerNew", maxBooks = 111})})
  (setBorrower (Just Borrower { name = "BorrowerNew", maxBooks = 111 }) bk2)

testSetBorrowerNothing = (~=?)
  (Book {title = "Title2", author = "Author2", borrower = Nothing})
  (setBorrower Nothing bk2)

testBookToStringSomeone = (~=?)
  "Title1 by Author1; Checked out to Borrower1"
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
