module Main (main) where

-- brsb = (brs, Bool)
-- bksb = (bks, Bool)

import Book
import Borrower
import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Library
import System.Directory

main :: IO ()
main = do
  tvBorrowers <- atomically (newTVar [])
  tvBooks <- atomically (newTVar [])
  atomically $ modifyTVar tvBorrowers (addItem (Borrower "Jim" 3))
  atomically $ modifyTVar tvBorrowers (addItem (Borrower "Sue" 3))
  atomically $
    modifyTVar tvBooks (addItem (Book "War And Peace" "Tolstoy" Nothing))
  atomically $
    modifyTVar
      tvBooks
      (addItem (Book "Great Expectations" "Dickens" Nothing))
  putStrLn "\nJust created new library"
  printStatus tvBooks tvBorrowers

  putStrLn "Check out War And Peace to Sue"
  borrowers <- readTVarIO tvBorrowers
  atomically $ modifyTVar tvBooks (checkOut "Sue" "War And Peace" borrowers)
  printStatus tvBooks tvBorrowers

  putStrLn "Now check in War And Peace from Sue..."
  atomically $ modifyTVar tvBooks (checkIn "War And Peace")
  putStrLn "...and check out Great Expectations to Jim"
  borrowers1 <- readTVarIO tvBorrowers
  atomically $
    modifyTVar tvBooks (checkOut "Jim" "Great Expectations" borrowers1)
  printStatus tvBooks tvBorrowers

  putStrLn "Add Eric and The Cat In The Hat"
  atomically $ modifyTVar tvBorrowers (addItem (Borrower "Eric" 1))
  atomically $
    modifyTVar
      tvBooks
      (addItem (Book "The Cat In The Hat" "Dr. Seuss" Nothing))
  putStrLn "Check Out Dr. Seuss to Eric"
  borrowers2 <- readTVarIO tvBorrowers
  atomically $
    modifyTVar tvBooks (checkOut "Eric" "The Cat In The Hat" borrowers2)
  printStatus tvBooks tvBorrowers

  putStrLn "Now let's do some BAD stuff..."

  putStrLn "Add a borrower that already exists (Borrower 'Jim' 3):"
  atomically $ modifyTVar tvBorrowers (addItem (Borrower "Jim" 3))
  printStatus tvBooks tvBorrowers
  resetV tvBooks tvBorrowers

  putStrLn
    "Add a book that already exists (Book 'War And Peace' 'Tolstoy' Nothing):"
  atomically $
    modifyTVar tvBooks (addItem (Book "War And Peace" "Tolstoy" Nothing))
  printStatus tvBooks tvBorrowers
  resetV tvBooks tvBorrowers

  putStrLn
    "Check out a valid book to an invalid person (checkOut 'JoJo' 'War And Peace' borrowers):"
  borrowers3 <- readTVarIO tvBorrowers
  atomically $ modifyTVar tvBooks (checkOut "JoJo" "War And Peace" borrowers3)
  printStatus tvBooks tvBorrowers
  resetV tvBooks tvBorrowers

  putStrLn
    "Check out an invalid book to an valid person (checkOut 'Sue' 'Not A Book' borrowers):"
  borrowers4 <- readTVarIO tvBorrowers
  atomically $ modifyTVar tvBooks (checkOut "Sue" "Not A Book" borrowers4)
  printStatus tvBooks tvBorrowers
  resetV tvBooks tvBorrowers

  putStrLn "Last - check in a book not checked out (checkIn 'War And Peace'):"
  atomically $ modifyTVar tvBooks (checkIn "War And Peace")
  printStatus tvBooks tvBorrowers

  putStrLn
    "Okay... let's finish with some persistence. First clear the whole library:"
  newEmptyV tvBooks tvBorrowers
  putStrLn
    "Lets read in a new library from \"borrowers-before.json\" and \"books-before.json\":"
  newV tvBooks tvBorrowers jsonBorrowersFileBefore jsonBooksFile
  putStrLn "Add... a new borrower:"
  atomically $ modifyTVar tvBorrowers (addItem (Borrower "BorrowerNew" 300))
  printStatus tvBooks tvBorrowers

  putStrLn "Save the revised borrowers to \"borrowers-after.json\""
  borrowers5 <- readTVarIO tvBorrowers
  let jsonBrsStr = borrowersToJsonString borrowers5
  writeFileFromJsonString jsonBrsStr jsonBorrowersFileAfter

  putStrLn "Clear the whole library again:"
  newEmptyV tvBooks tvBorrowers

  putStrLn
    "Then read in the revised library from \"borrowers-after.json\" and \"books-before.json\":"
  newV tvBooks tvBorrowers jsonBorrowersFileAfter jsonBooksFile

  putStrLn "Last... delete the file \"borrowers-after.json\""
  removeFile jsonBorrowersFileAfter
  newEmptyV tvBooks tvBorrowers

  putStrLn
    "Then try to make a library using the deleted \"borrowers-after.json\":"
  newV tvBooks tvBorrowers jsonBorrowersFileAfter jsonBorrowersFileAfter

  putStrLn
    "And if we read in a file with mal-formed json content... like \"bad-borrowers.json\":"
  newV tvBooks tvBorrowers jsonBorrowersFileBad jsonBorrowersFileAfter

  putStrLn "Or how about reading in an empty file... \"empty.json\":"
  newV tvBooks tvBorrowers emptyFile emptyFile

  putStrLn "And... that's all..."
  putStrLn "Thanks - bye!\n"

printStatus :: TVar Books -> TVar Borrowers -> IO ()
printStatus tvbks tvbrs = do
  bks <- readTVarIO tvbks
  brs <- readTVarIO tvbrs
  putStrLn (statusToString bks brs)

resetV :: TVar Books -> TVar Borrowers -> IO ()
resetV tvbks tvbrs = do
  bks <- readTVarIO tvbks
  brs <- readTVarIO tvbrs
  atomically $ writeTVar tvbks bks
  atomically $ writeTVar tvbrs brs
  putStrLn "Reset! --- All reset?..."
  printStatus tvbks tvbrs

newV :: TVar Books -> TVar Borrowers -> FilePath -> FilePath -> IO ()
newV tvbks tvbrs brsfl bksfl = do
  jsonBrsStr <- readFileIntoJsonString brsfl
  jsonBksStr <- readFileIntoJsonString bksfl
  let ebrs = jsonStringToBorrowers jsonBrsStr
      ebks = jsonStringToBooks jsonBksStr
  case ebrs of
    Right r -> atomically $ writeTVar tvbrs r
    Left l -> putStrLn l
  case ebks of
    Right r -> atomically $ writeTVar tvbks r
    Left l -> putStrLn l
  printStatus tvbks tvbrs

readFileIntoJsonString :: FilePath -> IO (Either ErrorString JsonString)
readFileIntoJsonString f = do
  result <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
  case result of
    Right r -> do
      let s = BS.unpack r
      return (Right s)
    Left _ -> return (Left "File read error.")

writeFileFromJsonString :: String -> FilePath -> IO ()
writeFileFromJsonString s f = BS.writeFile f bs where bs = BS.pack s

newEmptyV :: TVar Books -> TVar Borrowers -> IO ()
newEmptyV tvbks tvbrs = do
  atomically $ writeTVar tvbks []
  atomically $ writeTVar tvbrs []
  printStatus tvbks tvbrs

jsonBorrowersFileBefore :: FilePath
jsonBorrowersFileBefore = "borrowers-before.json"

jsonBorrowersFileAfter :: FilePath
jsonBorrowersFileAfter = "borrowers-after.json"

jsonBooksFile :: FilePath
jsonBooksFile = "books-before.json"

jsonBorrowersFileBad :: FilePath
jsonBorrowersFileBad = "bad-borrowers.json"

emptyFile :: FilePath
emptyFile = "empty.json"
