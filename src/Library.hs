{-# LANGUAGE NamedFieldPuns #-}

module Library where

-- br = Borrower
-- brs = [br]
-- brsb = (brs, Bool)
-- bk = Book
-- bks = [bk]
-- bksb = (bks, Bool)

import           Book
import           Borrower
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Maybe

type Borrowers = ([Borrower], Bool)
type Books = ([Book], Bool)

addBorrower :: Borrower -> Borrowers -> Borrowers
addBorrower br brsb = if null coll then (brs ++ [br], True) else (brs, False)
  where brs = fst brsb
        coll = filter (== br) brs

addBook :: Book -> Books -> Books
addBook tbk bksb = if null coll then (bks ++ [tbk], True) else (bks, False)
  where bks = fst bksb
        coll = filter (== tbk) bks

removeBook :: Book -> Books -> Books
removeBook tbk bksb = if not (null coll) then ([ bk | bk <- bks, bk /= tbk], True) else (bks, False)
  where bks = fst bksb
        coll = filter (== tbk) bks

findBook :: Title -> Books -> Maybe Book
findBook t bksb = if null coll then Nothing else Just (head coll)
  where coll = [ bk | bk <- bks, getTitle bk == t ]
        bks = fst bksb

findBorrower :: Name -> Borrowers -> Maybe Borrower
findBorrower n brsb = if null coll then Nothing else Just (head coll)
  where coll = [ br | br <- brs, getName br == n ]
        brs = fst brsb

getBooksForBorrower :: Borrower -> Books -> [Book]
getBooksForBorrower br bksb = [bk | bk <- bks, getBorrower bk == Just br]
  where bks = fst bksb

checkOut :: Name -> Title -> Borrowers -> Books -> Books
checkOut n t brsb bksb =
  if isJust mbk && isJust mbr && notMaxedOut && bookNotOut
    then addBook newBook fewerBooks
    else (bks, False)
      where bks = fst bksb
            mbk = findBook t bksb
            mbr = findBorrower n brsb
            booksOut = length (getBooksForBorrower (fromJust mbr) bksb)
            maxBooksAllowed = getMaxBooks (fromJust mbr)
            notMaxedOut = booksOut < maxBooksAllowed
            bookNotOut = isNothing (getBorrower (fromJust mbk))
            newBook = setBorrower mbr (fromJust mbk)
            fewerBooks = removeBook (fromJust mbk) bksb

checkIn :: Title -> Books -> Books
checkIn t bksb =
  if isJust mbk && bookOut
    then addBook newBook fewerBooks
    else (bks, False)
      where bks = fst bksb
            mbk = findBook t bksb
            bookOut = isJust (getBorrower (fromJust mbk))
            newBook = setBorrower Nothing (fromJust mbk)
            fewerBooks = removeBook (fromJust mbk) bksb

libraryToString :: Books -> Borrowers -> String
libraryToString bksb brsb = "Test Library: " ++
  show (length bks) ++ " books; " ++
  show (length brs) ++ " people."
    where bks = fst bksb
          brs = fst brsb

statusToString :: Books -> Borrowers -> String
statusToString bksb brsb = "\n" ++
  "--- Status Report of Test Library ---\n" ++
  "\n" ++
  libraryToString bksb brsb ++
  "\n" ++
  unlines (map bookToString bks) ++ "\n" ++
  unlines (map borrowerToString brs) ++ "\n" ++
  "--- End of Status Report ---" ++
  "\n"
    where bks = fst bksb
          brs = fst brsb
