module Library where

-- br = Borrower
-- brs = [br]
-- bk = Book
-- bks = [bk]

import           Book
import           Borrower
import           Data.Aeson            as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import           Data.Maybe

type Borrowers = [Borrower]
type Books = [Book]
type JsonString = String
type ErrorString = String

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

jsonStringToBorrowers :: Either ErrorString JsonString -> Either ErrorString Borrowers
jsonStringToBorrowers s =
  case s of
    Right r -> do
                 let brs = A.eitherDecodeStrict (BS.pack r) :: Either ErrorString Borrowers
                 case brs of
                   Right c -> Right c
                   Left _  -> Left "JSON parse error."
    Left l -> Left l

jsonStringToBooks :: Either ErrorString JsonString -> Either ErrorString Books
jsonStringToBooks s =
  case s of
    Right r -> do
                 let brs = A.eitherDecodeStrict (BS.pack r) :: Either ErrorString Books
                 case brs of
                   Right c -> Right c
                   Left _  -> Left "JSON parse error."
    Left l -> Left l

borrowersToJsonString :: Borrowers -> JsonString
borrowersToJsonString brs =
  BS.unpack (BL.toStrict $ A.encode brs)

booksToJsonString :: Books -> JsonString
booksToJsonString bks =
  BS.unpack (BL.toStrict $ A.encode bks)

libraryToString :: Books -> Borrowers -> String
libraryToString bks brs = "Test Library: " `mappend`
  show (length bks) `mappend`
  " books; " `mappend`
  show (length brs) `mappend`
  " borrowers."

statusToString :: Books -> Borrowers -> String
statusToString bks brs = "\n" `mappend`
  "--- Status Report of Test Library ---\n" `mappend`
  "\n" `mappend`
  libraryToString bks brs `mappend`
  "\n" `mappend`
  unlines (map bookToString bks) `mappend`
  "\n" `mappend`
  unlines (map borrowerToString brs) `mappend`
--  fmap unlines (borrowerToString brs) `mappend`
  "\n" `mappend`
  "--- End of Status Report ---" `mappend`
  "\n"

--vv = unlines (map borrowerToString brs)
