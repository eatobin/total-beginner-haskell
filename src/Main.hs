module Main where

import           All_Tests
import           Book
import           Book_Test
import           Library
import           Library_Test
import           Borrower
import           Borrower_Test

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

--main :: IO ()
--main = do
--  putStrLn "hello eric"

main :: IO ()
main = do
  tvBorrowers <- atomically (newTVar ([], True))
  tvBooks <- atomically (newTVar ([], True))
  atomically $ modifyTVar tvBorrowers (addBorrower (makeBorrower "Jim" 3))
  atomically $ modifyTVar tvBorrowers (addBorrower (makeBorrower "Sue" 3))
  atomically $ modifyTVar tvBooks (addBook (makeBook "War And Peace" "Tolstoy" Nothing))
  atomically $ modifyTVar tvBooks (addBook (makeBook "Great Expectations" "Dickens" Nothing))
  books <- atomRead tvBooks
  borrowers <- atomRead tvBorrowers
  putStrLn ""
  putStrLn "Just created new library"
  if (snd books) && (snd borrowers) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with the borrowers list or the books list - or both! ***\n"
  putStrLn "Check out War And Peace to Sue"
  atomically $ modifyTVar tvBooks (checkOut "Sue" "War And Peace" borrowers)
  books <- atomRead tvBooks
  if (snd books) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with a book check out or check in! ***\n"
  putStrLn "Now check in War And Peace from Sue..."
  atomically $ modifyTVar tvBooks (checkIn "War And Peace")
  putStrLn "...and check out Great Expectations to Jim"
  atomically $ modifyTVar tvBooks (checkOut "Jim" "Great Expectations" borrowers)
  books <- atomRead tvBooks
  if (snd books) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with a book check out or check in! ***\n"
  putStrLn "Add Eric and The Cat In The Hat"
  atomically $ modifyTVar tvBorrowers (addBorrower (makeBorrower "Eric" 1))
  atomically $ modifyTVar tvBooks (addBook (makeBook "The Cat In The Hat" "Dr. Seuss" Nothing))
  putStrLn "Check Out Dr. Seuss to Eric"
  borrowers <- atomRead tvBorrowers
  atomically $ modifyTVar tvBooks (checkOut "Eric" "The Cat In The Hat" borrowers)
  books <- atomRead tvBooks
  if (snd books) && (snd borrowers) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with the borrowers list or the books list - or both! ***\n"
  putStrLn "Now let's do some BAD stuff..."
  putStrLn "Add a borrower that already exists (makeBorrower 'Jim' 3):"
  atomically $ modifyTVar tvBorrowers (addBorrower (makeBorrower "Jim" 3))
  borrowers <- atomRead tvBorrowers
  if (snd books) && (snd borrowers) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with the borrowers list or the books list - or both! ***\n"
  atomically $ writeTVar tvBorrowers (fst borrowers, True)
  putStrLn "Add a book that already exists (makeBook 'War And Peace' 'Tolstoy' Nothing):"
  atomically $ modifyTVar tvBooks (addBook (makeBook "War And Peace" "Tolstoy" Nothing))
  books <- atomRead tvBooks
  if (snd books) && (snd borrowers) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with the borrowers list or the books list - or both! ***\n"
  atomically $ writeTVar tvBooks (fst books, True)
  books <- atomRead tvBooks
  borrowers <- atomRead tvBorrowers
  putStrLn "All reset?..."
  if (snd books) && (snd borrowers) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with the borrowers list or the books list - or both! ***\n"
  putStrLn "Check out a valid book to an invalid person (checkOut 'JoJo' 'War And Peace' borrowers):"
  atomically $ modifyTVar tvBooks (checkOut "JoJo" "War And Peace" borrowers)
  books <- atomRead tvBooks
  if (snd books) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with a book check out or check in! ***\n"
  putStrLn "Check out an invalid book to an valid person (checkOut 'Sue' 'Not A Book' borrowers):"
  atomically $ modifyTVar tvBooks (checkOut "Sue" "Not A Book" borrowers)
  books <- atomRead tvBooks
  if (snd books) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with a book check out or check in! ***\n"
  atomically $ writeTVar tvBooks (fst books, True)
  books <- atomRead tvBooks
  putStrLn "All reset?..."
  if (snd books) && (snd borrowers) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with the borrowers list or the books list - or both! ***\n"
  putStrLn "Last - check in a book not checked out (checkIn 'War And Peace'):"
  atomically $ modifyTVar tvBooks (checkIn "War And Peace")
  books <- atomRead tvBooks
  if (snd books) then putStrLn (statusToString books borrowers)
    else putStrLn "\n*** There is an error with a book check out or check in! ***\n"
  putStrLn "Thanks - bye!\n"

--module Main where


--- main :: IO ()
--- main = do shared <- atomically (newTVar (addBorrower (makePerson "Wowzer" 5) []))
          -- shared <- atomically (newTVar (makePerson "Wowzer" 5))
          -- before <- atomRead shared
          -- before <- atomically (readTVar shared)
          -- putStrLn $ "Before: " ++ show before
          --putStrLn (show (setName "Newish" before))
          --putStrLn $ "Next: " ++ show next
          --appV (+ 22) shared
          --dispVar shared
          --appV (* 3) shared
          --after <- atomRead shared
          --after <- atomically (readTVar shared)
          --putStrLn ("After: " ++ show after)after <- atomically (readTVar shared)
          --putStrLn ("After: " ++ show after)
          --putStrLn ("After: " ++ personToString after)
          --atomically (writeTVar shared 59)
          --appV (+22) shared
          --atomically (readTVar shared >>= \j -> writeTVar shared (j + 100))
          -- atomically (readTVar shared >>= \p -> writeTVar shared (setName "Changed" p))
          -- appV (setName "Changed2") shared
          --atomically (readTVar shared >>= \p -> writeTVar shared (setMaxtvBooks 88 p))
          -- appV (setMaxtvBooks 89) shared
          ---appV (addBorrower (makePerson "Eric" 59)) shared
          -- atomically (readTVar shared) >>= print
          ---dispVar shared
          -- putStrLn "Bye!"

atomRead = atomically . readTVar
dispVar x = atomRead x >>= print
--appV fn x = atomically $ readTVar x >>= writeTVar x . fn
--appV (addBook (makeBook "Great Expectations" "Dickens" Nothing)) tvBooks
--appV fn x = atomically (readTVar x >>= writeTVar x . fn)
--appV x = atomically (readTVar x >>= writeTVar x . (+ 1))
--appV x = atomically (readTVar x >>= \j -> writeTVar x (j + 20))
--appV x = atomically (readTVar x >>= \j -> writeTVar x (j + 20))
-- writeToTvBooksBool = atomically (readTVar tvBooks >>= \bs -> writeTVar tvBooksBool (bs, True))
--  atomically (readTVar tvBooks >>= \bs -> writeTVar tvBooksBool (bs, True))
