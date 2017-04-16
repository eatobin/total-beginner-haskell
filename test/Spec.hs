import Test.Hspec

import           Borrower
import           Book

br1 :: Borrower
br1 = Borrower { name = "Borrower1"
               , maxBooks = 1 }

bk1 :: Book
bk1 = Book { title = "Title1"
           , author = "Author1"
           , borrower = Just br1 }

bk2 :: Book
bk2 = Book { title = "Title2"
           , author = "Author2"
           , borrower = Nothing }

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
