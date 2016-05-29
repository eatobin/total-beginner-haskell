{-# LANGUAGE NamedFieldPuns #-}

module Book where

-- br = Borrower
-- bk = Book

import           Borrower
import           Data.Maybe

type Title = String
type Author = String

data Book = Book
  { title    :: Title
  , author   :: Author
  , borrower :: Maybe Borrower
  } deriving (Show, Eq)

makeBook :: Title -> Author -> Maybe Borrower -> Book
makeBook = Book

getTitle :: Book -> Title
getTitle Book {title} = title

getAuthor :: Book -> Author
getAuthor Book {author} = author

getBorrower :: Book -> Maybe Borrower
getBorrower Book {borrower} = borrower

setBorrower :: Maybe Borrower -> Book -> Book
setBorrower br bk@Book {borrower} = bk {borrower = br}

availableString :: Book -> String
availableString bk
  | isNothing br = "Available"
  | otherwise = "Checked out to " ++ getName (fromJust br)
  where
    br = getBorrower bk

bookToString :: Book -> String
bookToString bk = getTitle bk ++
  " by " ++ getAuthor bk ++
  "; " ++ availableString bk
