import           Test.Hspec

import           Book
import           Borrower
import           Library

br1 :: Borrower
br1 = Borrower { name = "Borrower1"
               , maxBooks = 1 }

br2 :: Borrower
br2 = Borrower { name = "Borrower2"
              , maxBooks = 2 }
br3 :: Borrower
br3 = Borrower { name = "Borrower3"
              , maxBooks = 3 }

brs1 :: [Borrower]
brs1 = [br1, br2]
brs2 :: [Borrower]
brs2 = [br3, br1, br2]

bk1 :: Book
bk1 = Book { title = "Title1"
           , author = "Author1"
           , borrower = Just br1 }

bk2 :: Book
bk2 = Book { title = "Title2"
           , author = "Author2"
           , borrower = Nothing }

bk3 :: Book
bk3 = Book { title = "Title3"
          , author = "Author3"
          , borrower = Just br3 }
bk4 :: Book
bk4 = Book { title = "Title4"
          , author = "Author4"
          , borrower = Just br3 }

bks1 :: [Book]
bks1 = [bk1, bk2]
bks2 :: [Book]
bks2 = [bk3, bk1, bk2]
bks3 :: [Book]
bks3 = [bk1, bk2, bk3, bk4]
bks5 :: [Book]
bks5 = [bk3, bk1, bk2]

jsonStringBorrowersBad :: String
jsonStringBorrowersBad = "[{\"name\"\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"
jsonStringBorrowers :: String
jsonStringBorrowers = "[{\"name\":\"Borrower1\",\"maxBooks\":1},{\"name\":\"Borrower2\",\"maxBooks\":2}]"
jsonStringBooks :: String
jsonStringBooks = "[{\"borrower\":{\"name\":\"Borrower1\",\"maxBooks\":1},\"author\":\"Author1\",\"title\":\"Title1\"},{\"borrower\":null,\"author\":\"Author2\",\"title\":\"Title2\"}]"

main :: IO ()
main = hspec $ do
  describe "Borrower tests" $ do
    it "testMakeBorrower" $
      makeBorrower "Borrower1" 1 `shouldBe` br1

    it "testGetName" $
      getName br1 `shouldBe` "Borrower1"

    it "testSetName" $
      setName "Borrower1" (Borrower "Jack" 1) `shouldBe` br1

    it "testGetMaxBooks" $
      getMaxBooks br1 `shouldBe` 1

    it "testSetMaxBooks" $
      setMaxBooks 11 br1
        `shouldBe` Borrower {name = "Borrower1", maxBooks = 11}

    it "testBorrowerToString" $
      borrowerToString br1 `shouldBe` "Borrower1 (1 books)"

  describe "Book tests" $ do
    it "testMakeBookNothing" $
      makeBook "Title2" "Author2" Nothing `shouldBe` bk2

    it "testMakeBookSomeone" $
      makeBook "Title1" "Author1" (Just (Borrower "Borrower1" 1))
        `shouldBe` bk1

    it "testGetTitle" $
      getTitle bk1 `shouldBe` "Title1"

    it "testGetAuthor" $
      getAuthor bk2 `shouldBe` "Author2"

    it "testGetBorrowerNothing" $
      getBorrower bk2 `shouldBe` Nothing

    it "testGetBorrowerSomeone" $
      getBorrower bk1 `shouldBe` Just Borrower { name = "Borrower1", maxBooks = 1 }

    it "testSetBorrowerSomeone" $
      setBorrower (Just Borrower { name = "BorrowerNew", maxBooks = 111 }) bk2
        `shouldBe` Book {title = "Title2", author = "Author2",
           borrower = Just Borrower {name = "BorrowerNew", maxBooks = 111}}

    it "testSetBorrowerNothing" $
      setBorrower Nothing bk1 `shouldBe`
        Book {title = "Title1", author = "Author1", borrower = Nothing}

    it "testBookToStringSomeone" $
      bookToString bk1 `shouldBe` "Title1 by Author1; Checked out to Borrower1"

    it "testBookToStringNothing" $
      bookToString bk2 `shouldBe` "Title2 by Author2; Available"

  describe "Library tests" $ do
    it "testAddBorrowerPass" $
      addItem br3 brs1 `shouldBe` brs2

    it "testAddBorrowerFail" $
      addItem br2 brs2 `shouldBe` brs2

    it "testAddBookPass" $
      addItem bk3 bks1 `shouldBe` bks2

    it "testAddBookFail" $
      addItem bk3 bks3 `shouldBe` bks3

    it "testRemoveBookPass" $
      removeBook bk3 bks2 `shouldBe` bks1

    it "testRemoveBookFail" $
      removeBook bk4 bks2 `shouldBe` bks5

    it "testFindBookPass" $
      findItem "Title4" bks3 getTitle `shouldBe` Just bk4

    it "testFindBookFail" $
      findItem "Title4" bks2 getTitle `shouldBe` Nothing

    it "testFindBorrowerPass" $
      findItem "Borrower3" brs2 getName `shouldBe` Just br3

    it "testFindBorrowerFail" $
      findItem "Borrower3" brs1 getName `shouldBe` Nothing

    it "testGetBooksForBorrower0books" $
      getBooksForBorrower br2 bks2 `shouldBe` []

    it "testGetBooksForBorrower1book" $
      getBooksForBorrower br1 bks2 `shouldBe` [bk1]

    it "testGetBooksForBorrower2books" $
      getBooksForBorrower br3 bks3 `shouldBe` [bk3, bk4]
