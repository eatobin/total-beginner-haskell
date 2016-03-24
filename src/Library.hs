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
addBorrower p b = b ++ [p]

getBorrower :: Name -> Borrowers -> Person
getBorrower n b = head $ [ p | p <- b, getName p == n ]
-- getBorrower n b = head $ filter (\p -> getName p == n) b

addBook :: Book -> Books -> Books
addBook b bs = bs ++ [b]

getBook :: Title -> Books -> Book
getBook t bs = head $ [ b | b <- bs, getTitle b == t ]

library :: IO ()
library = do shared <- atomically (newTVar (addBorrower (Person "First" 1) [Person "Zero" 0]))
             before <- atomically (readTVar shared)
             putStrLn $ "Before: " ++ show before
             --atomically (readTVar shared >>= \p -> writeTVar shared (getBorrower "First" p))
             --me <- atomically (readTVar shared >>= return \p -> (getBorrower "First" p))
             atomically (readTVar shared) >>= \p -> print (getBorrower "First" p)
             --boss2 <- getBorrower "First" boss
             --atomically (readTVar shared) >>= print
             --putStrLn (show boss)
             --putStrLn "Bye!"
