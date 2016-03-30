module Person_Test where

import Person
import Test.HUnit

p1 = Person {name = "Person1", maxBooks = 1}

testMakePerson = (~=?)
  p1
  (makePerson "Person1" 1)

testGetName = (~=?)
  "Person1"
  (getName p1)

testSetName = (~=?)
  Person {name = "Person1", maxBooks = 1}
  (setName "Person1" (Person "Jack" 1))

testGetMaxBooks = (~=?)
  1
  (getMaxBooks p1)

testSetMaxBooks = (~=?)
  Person {name = "Person1", maxBooks = 11}
  (setMaxBooks 11 p1)

testPersonToString = (~=?)
  "Person1 (1 books)"
  (personToString p1)

personTests = TestList [ testMakePerson, testGetName
                       , testSetName, testGetMaxBooks
                       , testSetMaxBooks, testPersonToString ]

runPersonTests = runTestTT $ TestList [ personTests ]
