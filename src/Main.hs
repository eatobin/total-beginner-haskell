module Main where

import           All_Tests
import           Book
import           Book_Test
import           Library
import           Library_Test
import           Person
import           Person_Test

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

-- main :: IO ()
-- main = do
--  putStrLn "hello eric"

main :: IO ()
main = do
  tvBorrowers <- atomically (newTVar [])
  tvBooks <- atomically (newTVar [])
  appV (addBook (makeBook "War And Peace" "Tolstoy" Nothing)) tvBooks
  appV (addBook (makeBook "Great Expectations" "Dickens" Nothing)) tvBooks
  appV (addBorrower (makePerson "Jim" 3)) tvBorrowers
  appV (addBorrower (makePerson "Sue" 3)) tvBorrowers
  books <- atomRead tvBooks
  borrowers <- atomRead tvBorrowers
  putStrLn ""
  putStrLn "Just created new library"
  putStrLn (statusToString books borrowers)
  putStrLn "Check out War And Peace to Sue"
  appV (checkOut "Sue" "War And Peace" borrowers) tvBooks
  books <- atomRead tvBooks
  putStrLn (statusToString books borrowers)
  putStrLn "Now check in War And Peace from Sue..."
  appV (checkIn "War And Peace") tvBooks
  putStrLn "...and check out Great Expectations to Jim"
  appV (checkOut "Jim" "Great Expectations" borrowers) tvBooks
  books <- atomRead tvBooks
  putStrLn (statusToString books borrowers)






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
appV fn x = atomically $ readTVar x >>= writeTVar x . fn
--appV fn x = atomically (readTVar x >>= writeTVar x . fn)
--appV x = atomically (readTVar x >>= writeTVar x . (+ 1))
--appV x = atomically (readTVar x >>= \j -> writeTVar x (j + 20))
--appV x = atomically (readTVar x >>= \j -> writeTVar x (j + 20))
