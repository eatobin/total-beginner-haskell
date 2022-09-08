-- λ> :load test/Spec.hs src/Borrower.hs src/Book.hs src/Library.hs
-- [1 of 4] Compiling Borrower         ( src/Borrower.hs, interpreted )
-- [2 of 4] Compiling Book             ( src/Book.hs, interpreted )
-- [3 of 4] Compiling Library          ( src/Library.hs, interpreted )
-- [4 of 4] Compiling Main             ( test/Spec.hs, interpreted )
-- Ok, four modules loaded.
-- *Main
-- λ>

import Book
import Borrower
import Library
import Test.Hspec

br1 :: Borrower
br1 = Borrower {name = "Borrower1", maxBooks = 1}

br2 :: Borrower
br2 = Borrower "Borrower2" 2

br3 :: Borrower
br3 = Borrower "Borrower3" 3

brs1 :: [Borrower]
brs1 = [br1, br2]

brs2 :: [Borrower]
brs2 = [br3, br1, br2]

bk1 :: Book
bk1 = Book {title = "Title1", author = "Author1", borrower = Just br1}

bk2 :: Book
bk2 = Book "Title2" "Author2" Nothing

bk3 :: Book
bk3 = Book "Title3" "Author3" (Just br3)

bk4 :: Book
bk4 = Book "Title4" "Author4" (Just br3)

bks1 :: [Book]
bks1 = [bk1, bk2]

bks2 :: [Book]
bks2 = [bk3, bk1, bk2]

bks3 :: [Book]
bks3 = [bk1, bk2, bk3, bk4]

bks5 :: [Book]
bks5 = [bk3, bk1, bk2]

jsonStringBorrowersBad :: String
jsonStringBorrowersBad =
  "[{\"name\"\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"

jsonStringBorrowers :: String
jsonStringBorrowers =
  "[{\"name\":\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"

jsonStringBooks :: String
jsonStringBooks =
  "[{\"borrower\":{\"name\":\"Borrower1\",\"maxBooks\":1},\"title\":\"Title1\",\"author\":\"Author1\"},{\"borrower\":null,\"title\":\"Title2\",\"author\":\"Author2\"}]"

main :: IO ()
main = hspec $ do
  describe "Borrower tests" $ do
    it "testBorrower" $ Borrower "Borrower1" 1 `shouldBe` br1

    it "testGetName" $ getName br1 `shouldBe` "Borrower1"

    it "testSetName" $ setName "Borrower1" (Borrower "Jack" 1) `shouldBe` br1

    it "testGetMaxBooks" $ getMaxBooks br1 `shouldBe` 1

    it "testSetMaxBooks" $
      setMaxBooks 11 br1
        `shouldBe` Borrower
          { name = "Borrower1",
            maxBooks = 11
          }

    it "testBorrowerToString" $
      borrowerToString br1
        `shouldBe` "Borrower1 (1 books)"

  describe "Book tests" $ do
    it "testBookNothing" $ Book "Title2" "Author2" Nothing `shouldBe` bk2

    it "testBookSomeone" $
      Book "Title1" "Author1" (Just (Borrower "Borrower1" 1))
        `shouldBe` bk1

    it "testGetTitle" $ getTitle bk1 `shouldBe` "Title1"

    it "testGetAuthor" $ getAuthor bk2 `shouldBe` "Author2"

    it "testGetBorrowerNothing" $ getBorrower bk2 `shouldBe` Nothing

    it "testGetBorrowerSomeone" $
      getBorrower bk1
        `shouldBe` Just
          Borrower
            { name = "Borrower1",
              maxBooks = 1
            }

    it "testSetBorrowerSomeone" $
      setBorrower (Just (Borrower "BorrowerNew" 111)) bk2
        `shouldBe` Book
          { title = "Title2",
            author = "Author2",
            borrower =
              Just
                Borrower
                  { name = "BorrowerNew",
                    maxBooks = 111
                  }
          }

    it "testSetBorrowerNothing" $
      setBorrower Nothing bk1
        `shouldBe` Book
          { title = "Title1",
            author = "Author1",
            borrower = Nothing
          }

    it "testBookToStringSomeone" $
      bookToString bk1
        `shouldBe` "Title1 by Author1; Checked out to Borrower1"

    it "testBookToStringNothing" $
      bookToString bk2
        `shouldBe` "Title2 by Author2; Available"

  describe "Library tests" $ do
    it "testAddBorrowerPass" $ addItem br3 brs1 `shouldBe` brs2

    it "testAddBorrowerFail" $ addItem br2 brs2 `shouldBe` brs2

    it "testAddBookPass" $ addItem bk3 bks1 `shouldBe` bks2

    it "testAddBookFail" $ addItem bk3 bks3 `shouldBe` bks3

    it "testRemoveBookPass" $ removeBook bk3 bks2 `shouldBe` bks1

    it "testRemoveBookFail" $ removeBook bk4 bks2 `shouldBe` bks5

    it "testFindBookPass" $ findItem "Title4" bks3 getTitle `shouldBe` Just bk4

    it "testFindBookFail" $ findItem "Title4" bks2 getTitle `shouldBe` Nothing

    it "testFindBorrowerPass" $
      findItem "Borrower3" brs2 getName
        `shouldBe` Just br3

    it "testFindBorrowerFail" $
      findItem "Borrower3" brs1 getName
        `shouldBe` Nothing

    it "testGetBooksForBorrower0books" $
      getBooksForBorrower br2 bks2
        `shouldBe` []

    it "testGetBooksForBorrower1book" $
      getBooksForBorrower br1 bks2
        `shouldBe` [bk1]

    it "testGetBooksForBorrower2books" $
      getBooksForBorrower br3 bks3
        `shouldBe` [bk3, bk4]

    it "testCheckOutFailCheckedOut" $
      checkOut "Borrower3" "Title1" brs2 bks1
        `shouldBe` [ Book
                       { title = "Title1",
                         author = "Author1",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower1",
                                 maxBooks = 1
                               }
                       },
                     Book
                       { title = "Title2",
                         author = "Author2",
                         borrower = Nothing
                       }
                   ]

    it "testCheckOutFailBadBook" $
      checkOut "Borrower3" "NoTitle" brs2 bks1
        `shouldBe` [ Book
                       { title = "Title1",
                         author = "Author1",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower1",
                                 maxBooks = 1
                               }
                       },
                     Book
                       { title = "Title2",
                         author = "Author2",
                         borrower = Nothing
                       }
                   ]

    it "testCheckOutFailBadBorrower" $
      checkOut "NoName" "Title1" brs2 bks1
        `shouldBe` [ Book
                       { title = "Title1",
                         author = "Author1",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower1",
                                 maxBooks = 1
                               }
                       },
                     Book
                       { title = "Title2",
                         author = "Author2",
                         borrower = Nothing
                       }
                   ]

    it "testCheckOutFailOverLimit" $
      checkOut "Borrower1" "Title2" brs2 bks1
        `shouldBe` [ Book
                       { title = "Title1",
                         author = "Author1",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower1",
                                 maxBooks = 1
                               }
                       },
                     Book
                       { title = "Title2",
                         author = "Author2",
                         borrower = Nothing
                       }
                   ]

    it "testCheckOutPass" $
      checkOut "Borrower3" "Title2" brs2 bks3
        `shouldBe` [ Book
                       { title = "Title2",
                         author = "Author2",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower3",
                                 maxBooks = 3
                               }
                       },
                     Book
                       { title = "Title1",
                         author = "Author1",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower1",
                                 maxBooks = 1
                               }
                       },
                     Book
                       { title = "Title3",
                         author = "Author3",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower3",
                                 maxBooks = 3
                               }
                       },
                     Book
                       { title = "Title4",
                         author = "Author4",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower3",
                                 maxBooks = 3
                               }
                       }
                   ]

    it "testCheckInPass" $
      checkIn "Title1" bks1
        `shouldBe` [ Book
                       { title = "Title1",
                         author = "Author1",
                         borrower = Nothing
                       },
                     Book
                       { title = "Title2",
                         author = "Author2",
                         borrower = Nothing
                       }
                   ]

    it "testCheckInFailCheckedIn" $
      checkIn "Title2" bks1
        `shouldBe` [ Book
                       { title = "Title1",
                         author = "Author1",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower1",
                                 maxBooks = 1
                               }
                       },
                     Book
                       { title = "Title2",
                         author = "Author2",
                         borrower = Nothing
                       }
                   ]

    it "testCheckInFailBadBook" $
      checkIn "NoTitle" bks1
        `shouldBe` [ Book
                       { title = "Title1",
                         author = "Author1",
                         borrower =
                           Just
                             Borrower
                               { name = "Borrower1",
                                 maxBooks = 1
                               }
                       },
                     Book
                       { title = "Title2",
                         author = "Author2",
                         borrower = Nothing
                       }
                   ]

    it "testJsonStringToBorrowersFail" $
      jsonStringToBorrowers (Right jsonStringBorrowersBad)
        `shouldBe` Left "JSON parse error."

    it "testJsonStringToBorrowersPass" $
      jsonStringToBorrowers (Right jsonStringBorrowers)
        `shouldBe` Right brs1

    it "testJsonStringToBooks" $
      jsonStringToBooks (Right jsonStringBooks)
        `shouldBe` Right bks1

    it "testBorrowersToJsonString" $
      borrowersToJsonString brs1
        `shouldBe` jsonStringBorrowers

    it "testBooksToJsonString" $
      booksToJsonString bks1
        `shouldBe` jsonStringBooks

    it "testLibraryToString" $
      libraryToString bks1 brs2
        `shouldBe` "Test Library: 2 books; 3 borrowers."

    it "testStatusToString" $
      statusToString bks2 brs2
        `shouldBe` "\n--- Status Report of Test Library ---\n\nTest Library: 3 books; 3 borrowers.\nTitle3 by Author3; Checked out to Borrower3\nTitle1 by Author1; Checked out to Borrower1\nTitle2 by Author2; Available\n\nBorrower3 (3 books)\nBorrower1 (1 books)\nBorrower2 (2 books)\n\n--- End of Status Report ---\n"
