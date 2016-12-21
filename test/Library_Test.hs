module Library_Test where

-- br = Borrower
-- brs = [br]
-- brsb = (brs, Bool)
-- bk = Book
-- bks = [bk]
-- bksb = (bks, Bool)

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

brsb1 = ([br1, br2], True)
brsb2 = ([br3, br1, br2], True)
brsb3 = ([br3, br1, br2], False)

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

bksb1 = ([bk1, bk2], True)
bksb2 = ([bk3, bk1, bk2], True)
bksb3 = ([bk1, bk2, bk3, bk4], True)
bksb4 = ([bk1, bk2, bk3, bk4], False)
bksb5 = ([bk3, bk1, bk2], False)

jsonStringBorrowersBad = "[{\"name\"\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"
jsonStringBorrowers = "[{\"name\":\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"
jsonStringBooks = "[{\"borrower\":{\"name\":\"Borrower1\",\"maxBooks\":1},\"author\":\"Author1\",\"title\":\"Title1\"},{\"borrower\":null,\"author\":\"Author2\",\"title\":\"Title2\"}]"

testAddBorrowerPass = (~=?)
  brsb2
  (addBorrower br3 brsb1)

testAddBorrowerFail = (~=?)
  brsb3
  (addBorrower br2 brsb2)

testAddBookPass = (~=?)
  bksb2
  (addBook bk3 bksb1)

testAddBookFail = (~=?)
  bksb4
  (addBook bk3 bksb3)

testRemoveBookPass = (~=?)
  bksb1
  (removeBook bk3 bksb2)

testRemoveBookFail = (~=?)
  bksb5
  (removeBook bk4 bksb2)

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

testCheckOutFailBadBook = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkOut "Borrower3" "NoTitle" brsb2 bksb1)

testCheckOutFailBadBorrower = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkOut "NoName" "Title1" brsb2 bksb1)

testCheckOutFailOverLimit = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkOut "Borrower1" "Title2" brsb2 bksb1)

testCheckOutPass = (~=?)
  ( [ Book {title = "Title2", author = "Author2", borrower = Just Borrower {name = "Borrower3", maxBooks = 3}}
    , Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title3", author = "Author3", borrower = Just Borrower {name = "Borrower3", maxBooks = 3}}
    , Book {title = "Title4", author = "Author4", borrower = Just Borrower {name = "Borrower3", maxBooks = 3}} ], True )
  (checkOut "Borrower3" "Title2" brsb2 bksb3)

testCheckInPass = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Nothing}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], True )
  (checkIn "Title1" bksb1)

testCheckInFailCheckedIn = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkIn "Title2" bksb1)

testCheckInFailBadBook = (~=?)
  ( [ Book {title = "Title1", author = "Author1", borrower = Just Borrower {name = "Borrower1", maxBooks = 1}}
    , Book {title = "Title2", author = "Author2", borrower = Nothing} ], False )
  (checkIn "NoTitle" bksb1)

testJsonStringToBorrowersFail = (~=?)
  ([], False)
  (jsonStringToBorrowers jsonStringBorrowersBad)

testJsonStringToBorrowersPass = (~=?)
  brsb1
  (jsonStringToBorrowers jsonStringBorrowers)

testJsonStringToBooks = (~=?)
  bksb1
  (jsonStringToBooks jsonStringBooks)

testBorrowersToJsonString = (~=?)
  jsonStringBorrowers
  (borrowersToJsonString brsb1)

testBooksToJsonString = (~=?)
  jsonStringBooks
  (booksToJsonString bksb1)

testLibraryToString = (~=?)
  "Test Library: 2 books; 3 borrowers."
  (libraryToString bksb1 brsb2)

testStatusToString = (~=?)
  "\n--- Status Report of Test Library ---\n\nTest Library: 3 books; 3 borrowers.\nTitle3 by Author3; Checked out to Borrower3\nTitle1 by Author1; Checked out to Borrower1\nTitle2 by Author2; Available\n\nBorrower3 (3 books)\nBorrower1 (1 books)\nBorrower2 (2 books)\n\n--- End of Status Report ---\n"
  (statusToString bksb2 brsb2)

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
                        , testJsonStringToBorrowersFail, testJsonStringToBooks
                        , testBorrowersToJsonString, testBooksToJsonString
                        , testJsonStringToBorrowersPass ]

runLibraryTests = runTestTT $ TestList [ libraryTests ]
