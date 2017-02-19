{-# OPTIONS -Wall #-}

module Borrower_Test where

import           Borrower
import           Test.HUnit

br1 :: Borrower
br1 = Borrower { name = "Borrower1"
               , maxBooks = 1 }

testMakeBorrower :: Test
testMakeBorrower = (~=?)
  br1
  (makeBorrower "Borrower1" 1)

testGetName :: Test
testGetName = (~=?)
  "Borrower1"
  (getName br1)

testSetName :: Test
testSetName = (~=?)
  br1
  (setName "Borrower1" (Borrower "Jack" 1))

testGetMaxBooks :: Test
testGetMaxBooks = (~=?)
  1
  (getMaxBooks br1)

testSetMaxBooks :: Test
testSetMaxBooks = (~=?)
  Borrower {name = "Borrower1", maxBooks = 11}
  (setMaxBooks 11 br1)

testBorrowerToString :: Test
testBorrowerToString = (~=?)
  "Borrower1 (1 books)"
  (borrowerToString br1)

borrowerTests :: Test
borrowerTests = TestList [ testMakeBorrower, testGetName
                         , testSetName, testGetMaxBooks
                         , testSetMaxBooks, testBorrowerToString ]

runBorrowerTests :: IO Counts
runBorrowerTests = runTestTT $ TestList [ borrowerTests ]
