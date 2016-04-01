{-# LANGUAGE NamedFieldPuns #-}

module Library where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Person
import Book

type LibraryName = String

data Library = Library { libName :: LibraryName
                       , libBorrowers :: [Person]
                       , libBooks :: [Book] } deriving (Show, Eq)

makeLibrary :: LibraryName -> [Person] -> [Book] -> Library
makeLibrary = Library

getLibName :: Library -> LibraryName
getLibName Library {libName} = libName

getLibBorrowers :: Library -> [Person]
getLibBorrowers Library {libBorrowers} = libBorrowers

getLibBooks :: Library -> [Book]
getLibBooks Library {libBooks} = libBooks

--addBorrower :: Person -> [Person] -> [Person]
--addBorrower p ps = ps ++ [p]

addBorrower :: Person -> Library -> Library
addBorrower p l = l {libBorrowers = nbrs}
  where nbrs = (getLibBorrowers l) ++ [p]

findBorrower :: Name -> [Person] -> Person
findBorrower n ps = head [ p | p <- ps, getName p == n ]
-- getBorrower n ps = head $ filter (\p -> getName p == n) ps

--addBook :: Book -> [Book] -> [Book]
--addBook b bs = bs ++ [b]

addBook :: Book -> Library -> Library
addBook b l = l {libBooks = nbks}
  where nbks = (getLibBooks l) ++ [b]

findBook :: Title -> [Book] -> Book
findBook t bs = head [ b | b <- bs, getTitle b == t ]

getBooksForPerson :: Person -> [Book] -> [Book]
getBooksForPerson p bs = [b | b <- bs, getBorrower b == (Just p)]

libraryToString :: Library -> String
libraryToString l = getLibName l ++ ": " ++ 
  show (length (getLibBooks l)) ++ " books; " ++
  show (length (getLibBorrowers l)) ++ " people."

--library :: IO ()
--library = do shared <- atomically (newTVar (addBorrower (Person "First" 1) [Person "Zero" 0]))
--             before <- atomically (readTVar shared)
--             putStrLn $ "Before: " ++ show before
--             --atomically (readTVar shared >>= \p -> writeTVar shared (getBorrower "First" p))
--             --me <- atomically (readTVar shared >>= return \p -> (getBorrower "First" p))
--             atomically (readTVar shared) >>= \p -> print (findBorrower "First" p)
--             --boss2 <- getBorrower "First" boss
--             --atomically (readTVar shared) >>= print
--             --putStrLn (show boss)
--             --putStrLn "Bye!"
