module Library_Test where

-- br = Borrower
-- brs = [br]
-- bk = Book
-- bks = [bk]

import           Book
import           Book_Test
import           Borrower
import           Borrower_Test
import           Library
import           Test.HUnit

-- br1 = Borrower { name = "Borrower1"
--                , maxBooks = 1 }
br2 = Borrower { name = "Borrower2"
               , maxBooks = 2 }
br3 = Borrower { name = "Borrower3"
               , maxBooks = 3 }

brs1 = [br1, br2]
brs2 = [br3, br1, br2]
brs3 = [br3, br1, br2]

-- bk1 = Book { title = "Title1"
--            , author = "Author1"
--            , borrower = Just br1 }
-- bk2 = Book { title = "Title2"
--            , author = "Author2"
--            , borrower = Nothing }
bk3 = Book { title = "Title3"
           , author = "Author3"
           , borrower = Just br3 }
bk4 = Book { title = "Title4"
           , author = "Author4"
           , borrower = Just br3 }

bks1 = [bk1, bk2]
bks2 = [bk3, bk1, bk2]
bks3 = [bk1, bk2, bk3, bk4]
bks4 = [bk1, bk2, bk3, bk4]
bks5 = [bk3, bk1, bk2]

jsonStringBorrowersBad = "[{\"name\"\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"
jsonStringBorrowers = "[{\"name\":\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"
jsonStringBooks = "[{\"borrower\":{\"name\":\"Borrower1\",\"maxBooks\":1},\"author\":\"Author1\",\"title\":\"Title1\"},{\"borrower\":null,\"author\":\"Author2\",\"title\":\"Title2\"}]"

testAddBorrowerPass = (~=?)
  brs2
  (addItem br3 brs1)

testAddBorrowerFail = (~=?)
  brs3
  (addItem br2 brs2)

testAddBookPass = (~=?)
  bks2
  (addItem bk3 bks1)

testAddBookFail = (~=?)
  bks4
  (addItem bk3 bks3)

testRemoveBookPass = (~=?)
  bks1
  (removeBook bk3 bks2)

testRemoveBookFail = (~=?)
  bks5
  (removeBook bk4 bks2)

testFindBookPass = (~=?)
  (Just bk4)
  (findItem "Title4" bks3 getTitle)

testFindBookFail = (~=?)
  Nothing
  (findItem "Title4" bks2 getTitle)

testFindBorrowerPass = (~=?)
  (Just br3)
  (findItem "Borrower3" brs2 getName)

testFindBorrowerFail = (~=?)
  Nothing
  (findItem "Borrower3" brs1 getName)

testGetBooksForBorrower0books = (~=?)
  []
  (getBooksForBorrower br2 bks2)

testGetBooksForBorrower1book = (~=?)
  [bk1]
  (getBooksForBorrower br1 bks2)

testGetBooksForBorrower2books = (~=?)
  [bk3, bk4]
  (getBooksForBorrower br3 bks3)

testCheckOutFailCheckedOut = (~=?)
  [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
  , Book {title = "Title2", author = "Author2", borrower = Nothing} ]
  (checkOut "Borrower3" "Title1" brs2 bks1)

testCheckOutFailBadBook = (~=?)
  [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
  , Book {title = "Title2", author = "Author2", borrower = Nothing} ]
  (checkOut "Borrower3" "NoTitle" brs2 bks1)

testCheckOutFailBadBorrower = (~=?)
  [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
  , Book {title = "Title2", author = "Author2", borrower = Nothing} ]
  (checkOut "NoName" "Title1" brs2 bks1)

testCheckOutFailOverLimit = (~=?)
  [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
  , Book {title = "Title2", author = "Author2", borrower = Nothing} ]
  (checkOut "Borrower1" "Title2" brs2 bks1)

testCheckOutPass = (~=?)
  [ Book {title = "Title2", author = "Author2", borrower = Just Borrower {name = "Borrower3", maxBooks = 3}}
  , Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
  , Book {title = "Title3", author = "Author3", borrower = Just Borrower {name = "Borrower3", maxBooks = 3}}
  , Book {title = "Title4", author = "Author4", borrower = Just Borrower {name = "Borrower3", maxBooks = 3}} ]
  (checkOut "Borrower3" "Title2" brs2 bks3)

testCheckInPass = (~=?)
  [ Book {title = "Title1", author = "Author1", borrower = Nothing}
  , Book {title = "Title2", author = "Author2", borrower = Nothing} ]
  (checkIn "Title1" bks1)

testCheckInFailCheckedIn = (~=?)
  [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
  , Book {title = "Title2", author = "Author2", borrower = Nothing} ]
  (checkIn "Title2" bks1)

testCheckInFailBadBook = (~=?)
  [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
  , Book {title = "Title2", author = "Author2", borrower = Nothing} ]
  (checkIn "NoTitle" bks1)

-- testJsonStringToBorrowersFail = (~=?)
--   []
--   (jsonStringToBorrowers jsonStringBorrowersBad)
--
-- testJsonStringToBorrowersPass = (~=?)
--   brs1
--   (jsonStringToBorrowers jsonStringBorrowers)
--
-- testJsonStringToBooks = (~=?)
--   bks1
--   (jsonStringToBooks jsonStringBooks)

testBorrowersToJsonString = (~=?)
  jsonStringBorrowers
  (borrowersToJsonString brs1)

testBooksToJsonString = (~=?)
  jsonStringBooks
  (booksToJsonString bks1)

testLibraryToString = (~=?)
  "Test Library: 2 books; 3 borrowers."
  (libraryToString bks1 brs2)

testStatusToString = (~=?)
  "\n--- Status Report of Test Library ---\n\nTest Library: 3 books; 3 borrowers.\nTitle3 by Author3; Checked out to Borrower3\nTitle1 by Author1; Checked out to Borrower1\nTitle2 by Author2; Available\n\nBorrower3 (3 books)\nBorrower1 (1 books)\nBorrower2 (2 books)\n\n--- End of Status Report ---\n"
  (statusToString bks2 brs2)

libraryTests = TestList [ testAddBorrowerPass, testAddBorrowerFail, testRemoveBookPass
                        , testRemoveBookFail, testCheckOutFailCheckedOut, testCheckOutFailOverLimit
                        , testAddBookPass, testAddBookFail, testGetBooksForBorrower0books
                        , testGetBooksForBorrower1book, testCheckOutPass
                        , testGetBooksForBorrower2books, testLibraryToString
                        , testCheckInPass, testCheckInFailCheckedIn
                        , testFindBookPass, testFindBookFail
                        , testFindBorrowerPass, testFindBorrowerFail
                        , testCheckOutFailBadBorrower, testCheckOutFailBadBook
                        , testCheckInFailBadBook, testStatusToString
                        -- , testJsonStringToBorrowersFail, testJsonStringToBooks
                        , testBorrowersToJsonString, testBooksToJsonString]
                        -- , testJsonStringToBorrowersPass ]

runLibraryTests = runTestTT $ TestList [ libraryTests ]
