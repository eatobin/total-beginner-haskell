{-# LANGUAGE NamedFieldPuns #-}

module Library_Test where

-- br = Borrower
-- brs = [br]
-- brsb = (brs, Bool)
-- bk = Book
-- bks = [bk]
-- bksb = (bks, Bool)

import           Book
import           Book_Test
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Library
import           Borrower
import           Borrower_Test
import           Test.HUnit

-- br1 = Borrower {name = "Borrower1", maxBooks = 1}
br2 = Borrower {name = "Borrower2", maxBooks = 2}
br3 = Borrower {name = "Borrower3", maxBooks = 3}

brsb1 = ([br1, br2], True)
brsb2 = ([br1, br2, br3], True)
brsb3 = ([br1, br2, br3], False)

-- bk1 = Book { title = "Title1"
--              , author = "Author1"
--              , borrower = Just br1 }
-- bk2 = Book { title = "Title2"
--              , author = "Author2"
--              , borrower = Nothing }
bk3 = Book { title = "Title3"
             , author = "Author3"
             , borrower = Just br3 }
bk4 = Book { title = "Title4"
             , author = "Author4"
             , borrower = Just br3 }

bksb1 = ([bk1, bk2], True)
bksb2 = ([bk1, bk2, bk3], True)
bksb3 = ([bk1, bk2, bk3, bk4], True)

--lib1 = Library { libName = "Test Library"
--                , libBorrowers = brsb1
--                , libBooks = bksb1 }

--lib2 = Library { libName = "Test Library"
--                , libBorrowers = brsb2
--                , libBooks = bksb1 }

--lib3 = Library { libName = "Test Library"
--                , libBorrowers = brsb1
--                , libBooks = bksb2 }

--lib4 = Library { libName = "Test Library"
--                , libBorrowers = brsb1
--                , libBooks = bksb3 }

--testMakeLibrary = (~=?)
--  lib1
--  (makeLibrary "Test Library" brsb1 bksb1)

--testGetLibName = (~=?)
--  "Test Library"
--  (getLibName lib2)

testAddBorrowerPass = (~=?)
  brsb2
  (addBorrower br3 brsb1)

testAddBorrowerFail = (~=?)
  brsb3
  (addBorrower br3 brsb3)

--testAddBorrower = (~=?)
--  lib2
--  (addBorrower br3 lib1)

testAddBook = (~=?)
  bksb2
  (addBook bk3 bksb1)

testRemoveBook = (~=?)
  bksb1
  (removeBook bk3 bksb2)

--testAddBook = (~=?)
--  lib3
--  (addBook bk3 lib1)

--testGetBooksForBorrower0books = (~=?)
--  []
--  (getBooksForBorrower br2 lib2)

--testGetBooksForBorrower1book = (~=?)
--  [bk1]
--  (getBooksForBorrower br1 lib2)

--testGetBooksForBorrower2books = (~=?)
--  [bk3, bk4]
--  (getBooksForBorrower br3 lib4)

testFindBookPass = (~=?)
  (Just bk4)
  (findBook "Title4" bksb3)

testFindBookFail = (~=?)
  Nothing
  (findBook "Title4" bksb2)

testFindBorrowerPass = (~=?)
  (Just br3)
  (findBorrower "Borrower3" brsb2)

testFindBorrowerFail = (~=?)
  Nothing
  (findBorrower "Borrower3" brsb1)

testGetBooksForBorrower0books = (~=?)
  []
  (getBooksForBorrower br2 bksb2)

testGetBooksForBorrower1book = (~=?)
  [bk1]
  (getBooksForBorrower br1 bksb2)

testGetBooksForBorrower2books = (~=?)
  [bk3, bk4]
  (getBooksForBorrower br3 bksb3)

testCheckOutFailCheckedOut = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkOut "Borrower3" "Title1" brsb2 bksb1)

testCheckOutFailOverLimit = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkOut "Borrower1" "Title2" brsb2 bksb1)

testCheckOutPass = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title3", author = "Author3", borrower = Just Borrower {name = "Borrower3", maxBooks = 3}}
    , Book {title = "Title4", author = "Author4", borrower = Just Borrower {name = "Borrower3", maxBooks = 3}}
    , Book {title = "Title2", author = "Author2", borrower = Just Borrower {name = "Borrower3", maxBooks = 3}} ], True )
  (checkOut "Borrower3" "Title2" brsb2 bksb3)

testCheckInPass = (~=?)
  ( [ Book {title = "Title2", author = "Author2", borrower = Nothing}
    , Book {title = "Title1", author = "Author1", borrower = Nothing} ], True )
  (checkIn "Title1" bksb1)

testCheckInFailCheckedIn = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkIn "Title2" bksb1)

testLibraryToString = (~=?)
  "Test Library: 2 books; 3 people."
  (libraryToString bksb1 brsb2)

libraryTests = TestList [ testAddBorrowerPass, testAddBorrowerFail, testRemoveBook
                        , testCheckOutFailCheckedOut, testCheckOutFailOverLimit
                        , testAddBook, testGetBooksForBorrower0books
                        , testGetBooksForBorrower1book, testCheckOutPass
                        , testGetBooksForBorrower2books, testLibraryToString
                        , testCheckInPass, testCheckInFailCheckedIn
                        , testFindBookPass, testFindBookFail
                        , testFindBorrowerPass, testFindBorrowerFail ]

runLibraryTests = runTestTT $ TestList [ libraryTests ]
