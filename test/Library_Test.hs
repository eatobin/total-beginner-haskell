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
ps1 = [p1, p2 ]
ps2 = [p1, p2, p3 ]

bk3 = Book { title = "Title3"
             , author = "Author3"
             , borrower = Just Person { name = "Borrower3", maxBooks = 3 } }
bks1 = [ bk1, bk2 ]
bks2 = [ bk1, bk2, bk3 ]

testaddBorrower = (~=?)
  ps2
  (addBorrower p3 ps1)

libraryTests = TestList [ testaddBorrower ]

runLibraryTests = runTestTT $ TestList [ libraryTests ]
