module All_Tests where

import Person_Test
import Book_Test
import Library_Test
import Test.HUnit

runAllTests = runTestTT $ TestList [ personTests, bookTests
                                   , libraryTests ]
