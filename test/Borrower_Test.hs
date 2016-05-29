module Borrower_Test where

import Borrower
import Test.HUnit

br1 = Borrower {name = "Borrower1", maxBooks = 1}

testMakeBorrower = (~=?)
  br1
  (makeBorrower "Borrower1" 1)

testGetName = (~=?)
  "Borrower1"
  (getName br1)

testSetName = (~=?)
  br1
  (setName "Borrower1" (Borrower "Jack" 1))

testGetMaxBooks = (~=?)
  1
  (getMaxBooks br1)

testSetMaxBooks = (~=?)
  Borrower {name = "Borrower1", maxBooks = 11}
  (setMaxBooks 11 br1)

testBorrowerToString = (~=?)
  "Borrower1 (1 books)"
  (borrowerToString br1)

borrowerTests = TestList [ testMakeBorrower, testGetName
                       , testSetName, testGetMaxBooks
                       , testSetMaxBooks, testBorrowerToString ]

runBorrowerTests = runTestTT $ TestList [ borrowerTests ]
