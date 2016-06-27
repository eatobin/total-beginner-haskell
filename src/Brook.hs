{-# LANGUAGE NamedFieldPuns #-}

module Brook where

type Name = String
type MaxBooks = Int
type Title = String
type Author = String

data Brook = Borrower
            { name     :: Name
            , maxBooks :: MaxBooks
            }
          | Book
            { title    :: Title
            , author   :: Author
            , borrower :: Maybe Brook
            } deriving (Show, Eq)

makeBrorrower :: Name -> MaxBooks -> Brook
makeBrorrower = Borrower

makeBrook :: Title -> Author -> Maybe Brook -> Brook
makeBrook = Book

getNamer :: Brook -> Maybe Name
getNamer Borrower {name} = Just name
getNamer _ = Nothing
