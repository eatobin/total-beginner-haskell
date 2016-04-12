{-# LANGUAGE NamedFieldPuns #-}

module Book where

import           Data.Maybe
import           Person

type Title = String
type Author = String

data Book = Book { title    :: Title
                 , author   :: Author
                 , borrower :: Maybe Person
                 } deriving (Show, Eq)

makeBook :: Title -> Author -> Maybe Person -> Book
makeBook = Book

getTitle :: Book -> Title
getTitle Book {title} = title

getAuthor :: Book -> Author
getAuthor Book {author} = author

setAuthor :: Author -> Book -> Book
setAuthor a bk@Book {author} = bk {author = a}

getBorrower :: Book -> Maybe Person
getBorrower Book {borrower} = borrower

setBorrower :: Maybe Person -> Book -> Book
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
