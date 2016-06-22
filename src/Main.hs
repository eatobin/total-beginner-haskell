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
  atomically $ writeTVar tvBooks ([], True)
  atomically $ writeTVar tvBorrowers ([], True)
  printStatus tvBooks tvBorrowers
  putStrLn "Lets read in a new library from \"borrowers-before.yml\" and \"books-before.yml\":"
  ymlBrsStr <- readFileIntoYamlString yamlBorrowersFile
  ymlBksStr <- readFileIntoYamlString yamlBooksFile
  let newBorrowers = yamlStringToBorrowrs ymlBrsStr
      newBooks = yamlStringToBooks ymlBksStr
  newV tvBooks tvBorrowers newBooks newBorrowers




  putStrLn "Thanks - bye!\n"

  ----dfe <- doesFileExist yamlBorrowersFile
  ----print dfe
  --ymlData <- BS.readFile yamlBorrowersFile
  --ymlData2 <- BS.readFile yamlBooksFile
  --let borrowers = Y.decode ymlData :: Maybe [Borrower]
  --    books = Y.decode ymlData2 :: Maybe [Book]
  ---- Print it, just for show
  --print $ fromJust borrowers
  --print $ fromJust books
  ----print ymlData
  ----print
  --ys1 <- readFileIntoYamlString yamlBorrowersFile
  --ys2 <- readFileIntoYamlString yamlBooksFile
  --ys3 <- readFileIntoYamlString "bad.txt"
  --putStrLn "\n\n"
  --print ys1
  --print ys2
  --print ys3

  --let newBorrowers = yamlStringToBorrowrs ys1
  --    newBooks = yamlStringToBooks ys2
  --newV tvBooks tvBorrowers newBooks newBorrowers





      --(println "Add... a new borrower:")
      --(swap! a-borrowers (partial add-borrower (make-borrower "BorrowerNew" 300)))
      --(print-status a-books a-borrowers)
      --(println "Save the revised borrowers to \"borrowers-after.yml\"")
      --(write-file-from-string (collection-to-yaml-string (deref a-borrowers)) yaml-borrowers-file-after)
      --(println "Clear the whole library again:")
      --(reset! a-borrowers [[] true])
      --(reset! a-books [[] true])
      --(print-status a-books a-borrowers)
      --(println "Then read in the revised library from \"borrowers-after.yml\" and \"books-before.yml\":")
      --(reset! a-borrowers (yaml-string-to-collection (read-file-into-string yaml-borrowers-file-after)))
      --(reset! a-books (yaml-string-to-collection (read-file-into-string yaml-books-file)))
      --(print-status a-books a-borrowers)
      --(println "Last... delete the file \"borrowers-after.yml\"")
      --(io/delete-file yaml-borrowers-file-after)
      --(println "Then try to make a library using the deleted \"borrowers-after.yml\":")
      --(reset! a-borrowers (yaml-string-to-collection (read-file-into-string yaml-borrowers-file-after)))
      --(reset! a-books (yaml-string-to-collection (read-file-into-string yaml-books-file)))
      --(print-status a-books a-borrowers)
      --(println "And if we read in a file with mal-formed yaml content... like \"bad-books.yml\":")
      --(reset! a-books (yaml-string-to-collection (read-file-into-string yaml-books-file-bad)))
      --(print-status a-books a-borrowers)
      --(println "Or how about reading in an empty file... \"empty.yml\":")
      --(reset! a-books (yaml-string-to-collection (read-file-into-string empty-file)))
      --(print-status a-books a-borrowers)
      --(println "And... that's all...")
      --(println "Thanks - bye!\n"))))




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

newV :: TVar ([Book], Bool) -> TVar ([Borrower], Bool) ->Books -> Borrowers -> IO ()
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
