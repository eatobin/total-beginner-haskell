module Person_Test where

import Person
import Test.HUnit

p = Person {name = "Sam", maxBooks = 7}

testMakePerson = (~=?)
  p
  (makePerson "Sam" 7)

testGetName = (~=?)
  "Sam"
  (getName p)

testSetName = (~=?)
  Person {name = "Sam", maxBooks = 7}
  (setName "Sam" (Person "Jack" 7))

testGetMaxBooks = (~=?)
  7
  (getMaxBooks p)

testSetMaxBooks = (~=?)
  Person {name = "Sam", maxBooks = 77}
  (setMaxBooks 77 p)

testPersonToString = (~=?)
  "Sam (7 books)"
  (personToString p)

personTests = TestList [ testMakePerson, testGetName
                       , testSetName, testGetMaxBooks
                       , testSetMaxBooks, testPersonToString ]

runPersonTests = runTestTT $ TestList [ personTests ]
