{-# OPTIONS -Wall #-}

module Book_Test where

import           Book
import           Borrower
import           Borrower_Test
import           Test.HUnit

--br1 = Borrower { name = "Borrower1"
--               , maxBooks = 1 }
bk1 :: Book
bk1 = Book { title = "Title1"
           , author = "Author1"
           , borrower = Just br1 }

bk2 :: Book
bk2 = Book { title = "Title2"
           , author = "Author2"
           , borrower = Nothing }

testMakeBookNothing :: Test
testMakeBookNothing = (~=?)
  bk2
  (makeBook "Title2" "Author2" Nothing)

testMakeBookSomeone :: Test
testMakeBookSomeone = (~=?)
  bk1
  (makeBook "Title1" "Author1" (Just (Borrower "Borrower1" 1)))

testGetTitle :: Test
testGetTitle = (~=?)
  "Title1"
  (getTitle bk1)

testGetAuthor :: Test
testGetAuthor = (~=?)
  "Author2"
  (getAuthor bk2)

testGetBorrowerNothing :: Test
testGetBorrowerNothing = (~=?)
  Nothing
  (getBorrower bk2)

testGetBorrowerSomeone :: Test
testGetBorrowerSomeone = (~=?)
  (Just Borrower { name = "Borrower1", maxBooks = 1 })
  (getBorrower bk1)

testSetBorrowerSomeone :: Test
testSetBorrowerSomeone = (~=?)
  Book {title = "Title2", author = "Author2",
         borrower = Just Borrower {name = "BorrowerNew", maxBooks = 111}}
  (setBorrower (Just Borrower { name = "BorrowerNew", maxBooks = 111 }) bk2)

testSetBorrowerNothing :: Test
testSetBorrowerNothing = (~=?)
  Book {title = "Title1", author = "Author1", borrower = Nothing}
  (setBorrower Nothing bk1)

testBookToStringSomeone :: Test
testBookToStringSomeone = (~=?)
  "Title1 by Author1; Checked out to Borrower1"
  (bookToString bk1)

testBookToStringNothing :: Test
testBookToStringNothing = (~=?)
  "Title2 by Author2; Available"
  (bookToString bk2)

bookTests :: Test
bookTests = TestList [ testMakeBookNothing, testMakeBookSomeone
                     , testGetTitle, testGetAuthor
                     , testGetBorrowerNothing
                     , testGetBorrowerSomeone, testSetBorrowerSomeone
                     , testSetBorrowerNothing, testBookToStringSomeone
                     , testBookToStringNothing ]

runBookTests :: IO Counts
runBookTests = runTestTT $ TestList [ bookTests ]
