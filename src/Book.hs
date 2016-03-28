{-# LANGUAGE NamedFieldPuns #-}

module Book where

import Data.Maybe
import Person

type Title = String
type Author = String

data Book = Book { title :: Title
                 , author :: Author
                 , borrower :: Maybe Person
                 } deriving (Show, Eq)

bk1 = makeBook "Title" "Author" (Just (Person "Borrower" 66))
bk2 = makeBook "Title22" "Author22" (Just (Person "Borrower22" 666))
bk = Book { title = "Title9", author = "Author8", borrower = Nothing }
bks = [ Book { title = "Title2"
             , author = "Author2"
             , borrower = Just Person { name = "ThreeX", maxBooks = 33 } }
      , Book { title = "Title3"
             , author = "Author3"
             , borrower = Just Person { name = "ThreeY", maxBooks = 39 } } ]

makeBook :: Title -> Author -> Maybe Person -> Book
makeBook = Book

getTitle :: Book -> Title
getTitle Book {title} = title

setTitle :: Title -> Book -> Book
setTitle t bk@Book {title} = bk {title = t}

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
