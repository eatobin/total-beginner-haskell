{-# LANGUAGE NamedFieldPuns #-}

module Library_Test where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Person
import Book
import Test.HUnit

bk1 = Book { title = "Title1"
             , author = "Author1"
             , borrower = Just Person { name = "Borrower1", maxBooks = 1 } }

bk2 = Book { title = "Title2"
             , author = "Author2"
             , borrower = Nothing }

bk3 = Book { title = "Title3"
             , author = "Author3"
             , borrower = Just Person { name = "Borrower3", maxBooks = 3 } }

bks = [ bk1, bk2 ]
