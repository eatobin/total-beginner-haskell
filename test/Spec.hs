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
    it "testMakeBorrower" $ do
      (makeBorrower "Borrower1" 1) `shouldBe` br1

    it "testGetName" $ do
      (getName br1) `shouldBe` "Borrower1"

    it "testSetName" $ do
      (setName "Borrower1" (Borrower "Jack" 1)) `shouldBe` br1

    it "testGetMaxBooks" $ do
      (getMaxBooks br1) `shouldBe` 1

    it "testSetMaxBooks" $ do
      (setMaxBooks 11 br1)
        `shouldBe` Borrower {name = "Borrower1", maxBooks = 11}

    it "testBorrowerToString" $ do
      (borrowerToString br1) `shouldBe` "Borrower1 (1 books)"

  describe "Book tests" $ do
    it "testMakeBookNothing" $ do
      (makeBook "Title2" "Author2" Nothing) `shouldBe` bk2

    it "testMakeBookSomeone" $ do
      (makeBook "Title1" "Author1" (Just (Borrower "Borrower1" 1)))
        `shouldBe` bk1
