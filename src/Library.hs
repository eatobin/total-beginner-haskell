{-# LANGUAGE NamedFieldPuns #-}

module Library where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Person
import Book

type Borrowers = [Person]
type Books = [Book]

b = [Person "One" 1, Person "Two" 2]
p = Person "Three" 3

addBorrower :: Person -> Borrowers -> Borrowers
addBorrower p brs = brs ++ [p]

findBorrower :: Name -> Borrowers -> Person
findBorrower n brs = head [ b | b <- brs, getName b == n ]
-- getBorrower n brs = head $ filter (\b -> getName b == n) brs

addBook :: Book -> Books -> Books
addBook bk bks = bks ++ [bk]

findBook :: Title -> Books -> Book
findBook t bks = head [ bk | bk <- bks, getTitle bk == t ]

library :: IO ()
library = do shared <- atomically (newTVar (addBorrower (Person "First" 1) [Person "Zero" 0]))
             before <- atomically (readTVar shared)
             putStrLn $ "Before: " ++ show before
             --atomically (readTVar shared >>= \p -> writeTVar shared (getBorrower "First" p))
             --me <- atomically (readTVar shared >>= return \p -> (getBorrower "First" p))
             atomically (readTVar shared) >>= \p -> print (findBorrower "First" p)
             --boss2 <- getBorrower "First" boss
             --atomically (readTVar shared) >>= print
             --putStrLn (show boss)
             --putStrLn "Bye!"
