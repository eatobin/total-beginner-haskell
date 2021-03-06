{-# OPTIONS -Wall #-}

module All_Tests where

import           Book_Test
import           Borrower_Test
import           Library_Test
import           Test.HUnit

runAllTests :: IO Counts
runAllTests = runTestTT $ TestList [ borrowerTests, bookTests
                                   , libraryTests ]
