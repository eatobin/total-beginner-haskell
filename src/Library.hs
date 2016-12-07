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
import           Data.Aeson            as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import           Data.Maybe
import           Data.Yaml             as Y

type Borrowers = ([Borrower], Bool)
type Books = ([Book], Bool)
type YamlString = String
type JsonString = String

addBorrower :: Borrower -> Borrowers -> Borrowers
addBorrower br brsb =
  if null coll
    then (br:brs, True)
    else (brs, False)
      where brs = fst brsb
            coll = filter (== br) brs

addBook :: Book -> Books -> Books
addBook bk bksb =
  if null coll
    then (bk:bks, True)
    else (bks, False)
      where bks = fst bksb
            coll = filter (== bk) bks

removeBook :: Book -> Books -> Books
removeBook tbk bksb =
  if not (null coll)
    then ([ bk | bk <- bks, bk /= tbk], True)
    else (bks, False)
      where bks = fst bksb
            coll = filter (== tbk) bks

findBook :: Title -> Books -> Maybe Book
findBook t bksb =
  if null coll
    then Nothing
    else Just (head coll)
      where coll = [ bk | bk <- bks, getTitle bk == t ]
            bks = fst bksb

findBorrower :: Name -> Borrowers -> Maybe Borrower
findBorrower n brsb =
  if null coll
    then Nothing
    else Just (head coll)
      where coll = [ br | br <- brs, getName br == n ]
            brs = fst brsb

getBooksForBorrower :: Borrower -> Books -> [Book]
getBooksForBorrower br bksb = [bk | bk <- bks, getBorrower bk == Just br]
  where bks = fst bksb

numBooksOut :: Borrower -> Books -> Int
numBooksOut br bksb = length (getBooksForBorrower br bksb)

notMaxedOut :: Borrower -> Books -> Bool
notMaxedOut br bksb = numBooksOut br bksb < getMaxBooks br

bookNotOut :: Book -> Bool
bookNotOut bk = isNothing (getBorrower bk)

bookOut :: Book -> Bool
bookOut bk = isJust (getBorrower bk)

checkOut :: Name -> Title -> Borrowers -> Books -> Books
checkOut n t brsb bksb =
  if isJust mbk && isJust mbr &&
      notMaxedOut (fromJust mbr) bksb && bookNotOut (fromJust mbk)
    then addBook newBook fewerBooks
    else (bks, False)
      where bks = fst bksb
            mbk = findBook t bksb
            mbr = findBorrower n brsb
            newBook = setBorrower mbr (fromJust mbk)
            fewerBooks = removeBook (fromJust mbk) bksb

checkIn :: Title -> Books -> Books
checkIn t bksb =
  if isJust mbk && bookOut (fromJust mbk)
    then addBook newBook fewerBooks
    else (bks, False)
      where bks = fst bksb
            mbk = findBook t bksb
            newBook = setBorrower Nothing (fromJust mbk)
            fewerBooks = removeBook (fromJust mbk) bksb

yamlStringToBorrowers :: YamlString -> Borrowers
yamlStringToBorrowers s =
  if isJust mbrs
    then (fromJust mbrs, True)
    else ([], False)
      where mbrs = Y.decode (BS.pack s) :: Maybe [Borrower]

jsonStringToBorrowers :: JsonString -> Borrowers
jsonStringToBorrowers s =
  if isJust mbrs
    then (fromJust mbrs, True)
    else ([], False)
      where mbrs = A.decode (BL.fromStrict $ BS.pack s) :: Maybe [Borrower]

yamlStringToBooks :: YamlString -> Books
yamlStringToBooks s =
  if isJust mbks
    then (fromJust mbks, True)
    else ([], False)
      where mbks = Y.decode (BS.pack s) :: Maybe [Book]

borrowersToYamlString :: Borrowers -> YamlString
borrowersToYamlString brsb =
  BS.unpack (Y.encode brs)
    where brs = fst brsb

booksToYamlString :: Books -> YamlString
booksToYamlString bksb =
  BS.unpack (Y.encode bks)
    where bks = fst bksb

libraryToString :: Books -> Borrowers -> String
libraryToString bksb brsb = "Test Library: " ++
  show (length bks) ++ " books; " ++
  show (length brs) ++ " borrowers."
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
