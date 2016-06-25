module Main where

-- brsb = (brs, Bool)
-- bksb = (bks, Bool)

import           All_Tests
import           Book
import           Book_Test
import           Borrower
import           Borrower_Test
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString.Char8  as BS
import           Data.Maybe
import           Data.Yaml              as Y
import           Library
import           Library_Test
import           System.Directory

main :: IO ()
main = do
  tvBorrowers <- atomically (newTVar ([], True))
  tvBooks <- atomically (newTVar ([], True))
  atomically $ modifyTVar tvBorrowers (addBorrower (makeBorrower "Jim" 3))
  atomically $ modifyTVar tvBorrowers (addBorrower (makeBorrower "Sue" 3))
  atomically $ modifyTVar tvBooks (addBook (makeBook "War And Peace" "Tolstoy" Nothing))
  atomically $ modifyTVar tvBooks (addBook (makeBook "Great Expectations" "Dickens" Nothing))
  putStrLn "\nJust created new library"
  printStatus tvBooks tvBorrowers
  putStrLn "Check out War And Peace to Sue"
  borrowers <- atomRead tvBorrowers
  atomically $ modifyTVar tvBooks (checkOut "Sue" "War And Peace" borrowers)
  printStatus tvBooks tvBorrowers
  putStrLn "Now check in War And Peace from Sue..."
  atomically $ modifyTVar tvBooks (checkIn "War And Peace")
  putStrLn "...and check out Great Expectations to Jim"
  borrowers <- atomRead tvBorrowers
  atomically $ modifyTVar tvBooks (checkOut "Jim" "Great Expectations" borrowers)
  printStatus tvBooks tvBorrowers
  putStrLn "Add Eric and The Cat In The Hat"
  atomically $ modifyTVar tvBorrowers (addBorrower (makeBorrower "Eric" 1))
  atomically $ modifyTVar tvBooks (addBook (makeBook "The Cat In The Hat" "Dr. Seuss" Nothing))
  putStrLn "Check Out Dr. Seuss to Eric"
  borrowers <- atomRead tvBorrowers
  atomically $ modifyTVar tvBooks (checkOut "Eric" "The Cat In The Hat" borrowers)
  printStatus tvBooks tvBorrowers
  putStrLn "Now let's do some BAD stuff..."
  putStrLn "Add a borrower that already exists (makeBorrower 'Jim' 3):"
  atomically $ modifyTVar tvBorrowers (addBorrower (makeBorrower "Jim" 3))
  printStatus tvBooks tvBorrowers
  resetV tvBooks tvBorrowers
  putStrLn "Add a book that already exists (makeBook 'War And Peace' 'Tolstoy' Nothing):"
  atomically $ modifyTVar tvBooks (addBook (makeBook "War And Peace" "Tolstoy" Nothing))
  printStatus tvBooks tvBorrowers
  resetV tvBooks tvBorrowers
  putStrLn "Check out a valid book to an invalid person (checkOut 'JoJo' 'War And Peace' borrowers):"
  borrowers <- atomRead tvBorrowers
  atomically $ modifyTVar tvBooks (checkOut "JoJo" "War And Peace" borrowers)
  printStatus tvBooks tvBorrowers
  resetV tvBooks tvBorrowers
  putStrLn "Check out an invalid book to an valid person (checkOut 'Sue' 'Not A Book' borrowers):"
  borrowers <- atomRead tvBorrowers
  atomically $ modifyTVar tvBooks (checkOut "Sue" "Not A Book" borrowers)
  printStatus tvBooks tvBorrowers
  resetV tvBooks tvBorrowers
  putStrLn "Last - check in a book not checked out (checkIn 'War And Peace'):"
  atomically $ modifyTVar tvBooks (checkIn "War And Peace")
  printStatus tvBooks tvBorrowers
  putStrLn "Okay... let's finish with some persistence. First clear the whole library:"
  newEmptyV tvBooks tvBorrowers
  putStrLn "Lets read in a new library from \"borrowers-before.yml\" and \"books-before.yml\":"
  ymlBrsStr <- readFileIntoYamlString yamlBorrowersFile
  ymlBksStr <- readFileIntoYamlString yamlBooksFile
  let newBorrowers = yamlStringToBorrowrs ymlBrsStr
      newBooks = yamlStringToBooks ymlBksStr
  newV tvBooks tvBorrowers newBooks newBorrowers
  putStrLn "Add... a new borrower:"
  atomically $ modifyTVar tvBorrowers (addBorrower (makeBorrower "BorrowerNew" 300))
  printStatus tvBooks tvBorrowers
  putStrLn "Save the revised borrowers to \"borrowers-after.yml\""
  borrowers <- atomRead tvBorrowers
  let ymlBrsStr = borrowersToYamlString borrowers
  writeFileFromYamlString ymlBrsStr "borrowers-after.yml"
  putStrLn "Clear the whole library again:"
  newEmptyV tvBooks tvBorrowers
  putStrLn "Then read in the revised library from \"borrowers-after.yml\" and \"books-before.yml\":"
  ymlBrsStr <- readFileIntoYamlString "borrowers-after.yml"
  ymlBksStr <- readFileIntoYamlString yamlBooksFile
  let newBorrowers = yamlStringToBorrowrs ymlBrsStr
      newBooks = yamlStringToBooks ymlBksStr
  newV tvBooks tvBorrowers newBooks newBorrowers
  putStrLn "Last... delete the file \"borrowers-after.yml\""
  removeFile "borrowers-after.yml"
  putStrLn "Then try to make a library using the deleted \"borrowers-after.yml\":"
  ymlBrsStr <- readFileIntoYamlString "borrowers-after.yml"
  ymlBksStr <- readFileIntoYamlString yamlBooksFile
  let newBorrowers = yamlStringToBorrowrs ymlBrsStr
      newBooks = yamlStringToBooks ymlBksStr
  newV tvBooks tvBorrowers newBooks newBorrowers
  putStrLn "And if we read in a file with mal-formed yaml content... like \"bad-borrowers.yml\":"
  ymlBrsStr <- readFileIntoYamlString "bad-borrowers.yml"
  let newBorrowers = yamlStringToBorrowrs ymlBrsStr
  newV tvBooks tvBorrowers newBooks newBorrowers
  putStrLn "Or how about reading in an empty file... \"empty.yml\":"
  ymlBrsStr <- readFileIntoYamlString "empty.yml"
  let newBorrowers = yamlStringToBorrowrs ymlBrsStr
  newV tvBooks tvBorrowers newBooks newBorrowers
  putStrLn "And... that's all..."
  putStrLn "Thanks - bye!\n"

atomRead :: TVar a -> IO a
atomRead = atomically . readTVar

printStatus :: TVar ([Book], Bool) -> TVar ([Borrower], Bool) -> IO ()
printStatus tvbksb tvbrsb = do
  bksb <- atomRead tvbksb
  brsb <- atomRead tvbrsb
  if snd bksb && snd brsb then putStrLn (statusToString bksb brsb)
    else putStrLn "\n*** There was an error with the operation just performed! ***\n"

resetV :: TVar ([Book], Bool) -> TVar ([Borrower], Bool) -> IO ()
resetV tvbksb tvbrsb = do
  bksb <- atomRead tvbksb
  brsb <- atomRead tvbrsb
  atomically $ writeTVar tvbksb (fst bksb, True)
  atomically $ writeTVar tvbrsb (fst brsb, True)
  putStrLn "Reset! --- All reset?..."
  printStatus tvbksb tvbrsb

newV :: TVar ([Book], Bool) -> TVar ([Borrower], Bool) -> Books -> Borrowers -> IO ()
newV tvbksb tvbrsb bksb brsb = do
  atomically $ writeTVar tvbksb bksb
  atomically $ writeTVar tvbrsb brsb
  printStatus tvbksb tvbrsb

yamlBorrowersFile :: FilePath
yamlBorrowersFile = "borrowers-before.yml"

yamlBooksFile :: FilePath
yamlBooksFile = "books-before.yml"

readFileIntoYamlString :: FilePath -> IO String
readFileIntoYamlString f = do
  dfe <- doesFileExist f
  if dfe
    then do
      bs <- BS.readFile f
      let s = BS.unpack bs
      return s
    else do
      let bs = BS.empty
          s = BS.unpack bs
      return s

writeFileFromYamlString :: String -> FilePath -> IO ()
writeFileFromYamlString s f =
  BS.writeFile f bs
    where
      bs = BS.pack s

newEmptyV :: TVar ([Book], Bool) -> TVar ([Borrower], Bool) -> IO ()
newEmptyV tvbksb tvbrsb = do
  atomically $ writeTVar tvbksb ([], True)
  atomically $ writeTVar tvbrsb ([], True)
  printStatus tvbksb tvbrsb




-- /Users/eatobin/haskell_projects/total-beginner-haskell/src/Main.hs: 74, 3
-- Reduce duplication
-- Found:
--   ymlBksStr <- readFileIntoYamlString yamlBooksFile
--   let newBorrowers = yamlStringToBorrowrs ymlBrsStr
--       newBooks = yamlStringToBooks ymlBksStr
--   newV tvBooks tvBorrowers newBooks newBorrowers
-- Why not:
--   Combine with /Users/eatobin/haskell_projects/total-beginner-haskell/src/Main.hs:89:3
-- /Users/eatobin/haskell_projects/total-beginner-haskell/src/Main.hs: 88, 3
-- Reduce duplication
-- Found:
--   ymlBrsStr <- readFileIntoYamlString "borrowers-after.yml"
--   ymlBksStr <- readFileIntoYamlString yamlBooksFile
--   let newBorrowers = yamlStringToBorrowrs ymlBrsStr
--       newBooks = yamlStringToBooks ymlBksStr
--   newV tvBooks tvBorrowers newBooks newBorrowers
-- Why not:
--   Combine with /Users/eatobin/haskell_projects/total-beginner-haskell/src/Main.hs:96:3
