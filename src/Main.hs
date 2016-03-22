--module Main where

--import Person
--import Person_Test

--main :: IO ()
--main = do
--  putStrLn "hello eric"



module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Person

main = do shared <- atomically (newTVar (makePerson "Wowzer" 5))
          --before <- atomRead shared
          before <- atomically (readTVar shared)
          putStrLn $ "Before: " ++ show before
          --putStrLn (show (setName "Newish" before))
          --putStrLn $ "Next: " ++ show next
          --appV (+ 22) shared
          --appV (setName "Changed") shared
          --appV (setMaxBooks 88) shared
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
          atomically (readTVar shared >>= \p -> writeTVar shared (setName "Changed" p))
          atomically (readTVar shared >>= \p -> writeTVar shared (setMaxBooks 88 p))
          atomically (readTVar shared) >>= print
          putStrLn "Bye!"

atomRead = atomically . readTVar
dispVar x = atomRead x >>= print
appV fn x = atomically $ readTVar x >>= writeTVar x . fn
--appV fn x = atomically (readTVar x >>= writeTVar x . fn)
--appV x = atomically (readTVar x >>= writeTVar x . (+ 1))
--appV x = atomically (readTVar x >>= \j -> writeTVar x (j + 20))
--appV x = atomically (readTVar x >>= \j -> writeTVar x (j + 20))
