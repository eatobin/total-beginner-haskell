{-# LANGUAGE NamedFieldPuns #-}

module Library_Test where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Person
import Person_Test
import Book
import Book_Test
import Library
import Test.HUnit

p2 = Person {name = "Person2", maxBooks = 2}
p3 = Person {name = "Person3", maxBooks = 3}
ps1 = [ p1, p2 ]
ps2 = [ p1, p2, p3 ]

bk3 = Book { title = "Title3"
             , author = "Author3"
             , borrower = Just p3 }
bk4 = Book { title = "Title4"
             , author = "Author4"
             , borrower = Just p3 }
bks1 = [ bk1, bk2 ]
bks2 = [ bk1, bk2, bk3 ]
bks3 = [ bk1, bk2, bk3, bk4 ]

lib1 = Library { libName = "Test Library"
                , libBorrowers = ps1
                , libBooks = bks1 }

lib2 = Library { libName = "Test Library"
                , libBorrowers = ps2
                , libBooks = bks1 }

lib3 = Library { libName = "Test Library"
                , libBorrowers = ps1
                , libBooks = bks2 }

testMakeLibrary = (~=?)
  lib1
  (makeLibrary "Test Library" ps1 bks1)

testGetLibName = (~=?)
  "Test Library"
  (getLibName lib2)

testAddBorrower = (~=?)
  lib2
  (addBorrower p3 lib1)

testAddBook = (~=?)
  lib3
  (addBook bk3 lib1)

testGetBooksForPerson0books = (~=?)
  []
  (getBooksForPerson p2 bks2)

testGetBooksForPerson1book = (~=?)
  [bk1]
  (getBooksForPerson p1 bks2)

testGetBooksForPerson2books = (~=?)
  [bk3, bk4]
  (getBooksForPerson p3 bks3)

libraryTests = TestList [ testMakeLibrary, testGetLibName, testAddBorrower
                        , testAddBook, testGetBooksForPerson0books
                        , testGetBooksForPerson1book
                        , testGetBooksForPerson2books ]

runLibraryTests = runTestTT $ TestList [ libraryTests ]
