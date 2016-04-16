{-# LANGUAGE NamedFieldPuns #-}

module Library_Test where

import           Book
import           Book_Test
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Library
import           Person
import           Person_Test
import           Test.HUnit

-- p1 = Person {name = "Person1", maxBooks = 1}
p2 = Person {name = "Person2", maxBooks = 2}
p3 = Person {name = "Person3", maxBooks = 3}

ps1 = [ p1, p2 ]
ps2 = [ p1, p2, p3 ]

-- bk1 = Book { title = "Title1"
--              , author = "Author1"
--              , borrower = Just p1 }
-- bk2 = Book { title = "Title2"
--              , author = "Author2"
--              , borrower = Nothing }
bk3 = Book { title = "Title3"
             , author = "Author3"
             , borrower = Just p3 }
bk4 = Book { title = "Title4"
             , author = "Author4"
             , borrower = Just p3 }

bks1 = [ bk1, bk2 ]
bks2 = [ bk1, bk2, bk3 ]
bks3 = [ bk1, bk2, bk3, bk4 ]

--lib1 = Library { libName = "Test Library"
--                , libBorrowers = ps1
--                , libBooks = bks1 }

--lib2 = Library { libName = "Test Library"
--                , libBorrowers = ps2
--                , libBooks = bks1 }

--lib3 = Library { libName = "Test Library"
--                , libBorrowers = ps1
--                , libBooks = bks2 }

--lib4 = Library { libName = "Test Library"
--                , libBorrowers = ps1
--                , libBooks = bks3 }

--testMakeLibrary = (~=?)
--  lib1
--  (makeLibrary "Test Library" ps1 bks1)

--testGetLibName = (~=?)
--  "Test Library"
--  (getLibName lib2)

testAddBorrower = (~=?)
  ps2
  (addBorrower p3 ps1)

--testAddBorrower = (~=?)
--  lib2
--  (addBorrower p3 lib1)

testAddBook = (~=?)
  bks2
  (addBook bk3 bks1)

testRemoveBook = (~=?)
  bks1
  (removeBook bk3 bks2)

--testAddBook = (~=?)
--  lib3
--  (addBook bk3 lib1)

--testGetBooksForPerson0books = (~=?)
--  []
--  (getBooksForPerson p2 lib2)

--testGetBooksForPerson1book = (~=?)
--  [bk1]
--  (getBooksForPerson p1 lib2)

--testGetBooksForPerson2books = (~=?)
--  [bk3, bk4]
--  (getBooksForPerson p3 lib4)

testFindBookPass = (~=?)
  (Just bk4)
  (findBook "Title4" bks3)

testFindBookFail = (~=?)
  Nothing
  (findBook "Title4" bks2)

testFindPersonPass = (~=?)
  (Just p3)
  (findPerson "Person3" ps2)

testFindPersonFail = (~=?)
  Nothing
  (findPerson "Person3" ps1)

testGetBooksForPerson0books = (~=?)
  []
  (getBooksForPerson p2 bks2)

testGetBooksForPerson1book = (~=?)
  [bk1]
  (getBooksForPerson p1 bks2)

testGetBooksForPerson2books = (~=?)
  [bk3, bk4]
  (getBooksForPerson p3 bks3)

testCheckOutFailCheckedOut = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Person {name = "Person1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkOut "Person3" "Title1" ps2 (bks1, False))

testCheckOutFailOverLimit = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Person {name = "Person1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkOut "Person1" "Title2" ps2 (bks1, True))

testCheckOutPass = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Person {name = "Person1", maxBooks = 1}}
    , Book {title = "Title3", author = "Author3", borrower = Just Person {name = "Person3", maxBooks = 3}}
    , Book {title = "Title4", author = "Author4", borrower = Just Person {name = "Person3", maxBooks = 3}}
    , Book {title = "Title2", author = "Author2", borrower = Just Person {name = "Person3", maxBooks = 3}} ], True )
  (checkOut "Person3" "Title2" ps2 (bks3, True))

testCheckInPass = (~=?)
  [ Book {title = "Title2", author = "Author2", borrower = Nothing}
  , Book {title = "Title1", author = "Author1", borrower = Nothing} ]
  (checkIn "Title1" bks1)

testCheckInFailCheckedIn = (~=?)
  [ Book {title = "Title1", author = "Author1", borrower = Just Person {name = "Person1", maxBooks = 1}}
  , Book {title = "Title2", author = "Author2", borrower = Nothing} ]
  (checkIn "Title2" bks1)

testLibraryToString = (~=?)
  "Test Library: 2 books; 3 people."
  (libraryToString bks1 ps2)

libraryTests = TestList [ testAddBorrower, testRemoveBook
                        , testCheckOutFailCheckedOut, testCheckOutFailOverLimit
                        , testAddBook, testGetBooksForPerson0books
                        , testGetBooksForPerson1book, testCheckOutPass
                        , testGetBooksForPerson2books, testLibraryToString
                        , testCheckInPass, testCheckInFailCheckedIn
                        , testFindBookPass, testFindBookFail
                        , testFindPersonPass, testFindPersonFail ]

runLibraryTests = runTestTT $ TestList [ libraryTests ]
