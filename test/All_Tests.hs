module All_Tests where

import Borrower_Test
import Book_Test
import Library_Test
import Test.HUnit

runAllTests = runTestTT $ TestList [ borrowerTests, bookTests
                                   , libraryTests ]
