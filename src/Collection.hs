{-# LANGUAGE NamedFieldPuns #-}

module Collection where

type Name = String
type MaxBooks = Int
type Title = String
type Author = String

data Item = Borrower
            { name     :: Name
            , maxBooks :: MaxBooks
            }
          | Book
            { title    :: Title
            , author   :: Author
            , borrower :: Maybe Item
            } deriving (Show, Eq)

makeBorrower :: Name -> MaxBooks -> Item
makeBorrower = Borrower

makeBook :: Title -> Author -> Maybe Item -> Item
makeBook = Book

getName :: Item -> Maybe Name
getName Borrower {name} = Just name
getName _ = Nothing
