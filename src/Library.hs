{-# LANGUAGE NamedFieldPuns #-}

module Library where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Person

type Borrowers = [Person]

b = [(Person "One" 1), (Person "Two" 2)]

addBorrower :: Person -> Borrowers -> Borrowers
addBorrower p b = b ++ [p]

getBorrower :: Name -> Borrowers -> Borrowers
getBorrower n b = [ p | p <- b, getName p == n ]
--getBorrower n b = filter (\p -> getName p == n) b



library :: IO ()
library = do shared <- atomically (newTVar (addBorrower (Person "First" 1) [(Person "Zero" 0)]))
             before <- atomically (readTVar shared)
             putStrLn $ "Before: " ++ show before
             --atomically (readTVar shared >>= \p -> writeTVar shared (getBorrower "First" p))
             --me <- atomically (readTVar shared >>= return \p -> (getBorrower "First" p))
             atomically (readTVar shared) >>= \p -> print (getBorrower "FirstX" p)
             --boss2 <- getBorrower "First" boss
             --atomically (readTVar shared) >>= print
             --putStrLn (show boss)
             putStrLn "Bye!"
