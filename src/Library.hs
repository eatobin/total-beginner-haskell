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

type Borrowers = [Borrower]
type Books = [Book]
type JsonString = String

-- addBorrower :: Borrower -> Borrowers -> Borrowers
-- addBorrower br brs =
--   if br `elem` brs
--     then brs
--     else br:brs

-- addBook :: Book -> Books -> Books
-- addBook bk bks =
--   if bk `elem` bks
--     then bks
--     else bk:bks

addItem :: (Eq a) => a -> [a] -> [a]
addItem x xs =
  if x `elem` xs
    then xs
    else x:xs

removeBook :: Book -> Books -> Books
removeBook bk bks =
  if bk `elem` bks
    then filter (/= bk) bks
    else bks

findItem :: String -> [a] -> (a -> String) -> Maybe a
findItem tgt xs f =
  if null result
    then Nothing
    else Just (head result)
      where result = [ x | x <- xs, f x == tgt ]

getBooksForBorrower :: Borrower -> Books -> [Book]
getBooksForBorrower br bks = [bk | bk <- bks, getBorrower bk == Just br]

numBooksOut :: Borrower -> Books -> Int
numBooksOut br bksb = length (getBooksForBorrower br bksb)

notMaxedOut :: Borrower -> Books -> Bool
notMaxedOut br bksb = numBooksOut br bksb < getMaxBooks br

bookNotOut :: Book -> Bool
bookNotOut bk = isNothing (getBorrower bk)

bookOut :: Book -> Bool
bookOut bk = isJust (getBorrower bk)

checkOut :: Name -> Title -> Borrowers -> Books -> Books
checkOut n t brs bks =
  if isJust mbk && isJust mbr &&
      notMaxedOut (fromJust mbr) bks && bookNotOut (fromJust mbk)
    then addItem newBook fewerBooks
    else bks
      where mbk = findItem t bks getTitle
            mbr = findItem n brs getName
            newBook = setBorrower mbr (fromJust mbk)
            fewerBooks = removeBook (fromJust mbk) bks

checkIn :: Title -> Books -> Books
checkIn t bks =
  if isJust mbk && bookOut (fromJust mbk)
    then addItem newBook fewerBooks
    else bks
      where mbk = findItem t bks getTitle
            newBook = setBorrower Nothing (fromJust mbk)
            fewerBooks = removeBook (fromJust mbk) bks

jsonStringToBorrowers :: JsonString -> Borrowers
jsonStringToBorrowers s =
  fromMaybe [] mbrs
    where mbrs = A.decodeStrict (BS.pack s) :: Maybe [Borrower]

jsonStringToBooks :: JsonString -> Books
jsonStringToBooks s =
  fromMaybe [] mbks
    where mbks = A.decodeStrict (BS.pack s) :: Maybe [Book]

borrowersToJsonString :: Borrowers -> JsonString
borrowersToJsonString brs =
  BS.unpack (BL.toStrict $ A.encode brs)

booksToJsonString :: Books -> JsonString
booksToJsonString bks =
  BS.unpack (BL.toStrict $ A.encode bks)

libraryToString :: Books -> Borrowers -> String
libraryToString bks brs = "Test Library: " ++
  show (length bks) ++ " books; " ++
  show (length brs) ++ " borrowers."

statusToString :: Books -> Borrowers -> String
statusToString bks brs = "\n" ++
  "--- Status Report of Test Library ---\n" ++
  "\n" ++
  libraryToString bks brs ++
  "\n" ++
  unlines (map bookToString bks) ++ "\n" ++
  unlines (map borrowerToString brs) ++ "\n" ++
  "--- End of Status Report ---" ++
  "\n"
