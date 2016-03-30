{-# LANGUAGE NamedFieldPuns #-}

module Library where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Person
import Book

addBorrower :: Person -> [Person] -> [Person]
addBorrower p ps = ps ++ [p]

findBorrower :: Name -> [Person] -> Person
findBorrower n ps = head [ p | p <- ps, getName p == n ]
-- getBorrower n ps = head $ filter (\p -> getName p == n) ps

addBook :: Book -> [Book] -> [Book]
addBook b bs = bs ++ [b]

findBook :: Title -> [Book] -> Book
findBook t bs = head [ b | b <- bs, getTitle b == t ]

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
