{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Book where

import           Borrower
import           Data.Aeson
import           Data.Maybe
import           GHC.Generics

-- br = Borrower
-- bk = Book

type Title = String
type Author = String

data Book = Book
  { title    :: Title
  , author   :: Author
  , borrower :: Maybe Borrower
  } deriving (Show, Eq, Generic)

instance FromJSON Book

instance ToJSON Book

getTitle :: Book -> Title
getTitle Book { title } = title

getAuthor :: Book -> Author
getAuthor Book { author } = author

getBorrower :: Book -> Maybe Borrower
getBorrower Book { borrower } = borrower

setBorrower :: Maybe Borrower -> Book -> Book
setBorrower mbr bk = bk { borrower = mbr }

availableString :: Book -> String
availableString bk | isNothing br = "Available"
                   | otherwise    = "Checked out to " ++ getName (fromJust br)
  where br = getBorrower bk

bookToString :: Book -> String
bookToString bk =
  getTitle bk ++ " by " ++ getAuthor bk ++ "; " ++ availableString bk
