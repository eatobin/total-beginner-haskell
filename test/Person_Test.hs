module Person_Test where

import Person
import Test.HUnit

testMakePerson = (~=?)
  Person {name = "Sam", maxBooks = 7}
  (makePerson "Sam" 7)

testGetName = (~=?)
  "Jack"
  (getName (Person "Jack" 7))

testSetName = (~=?)
  Person {name = "Sam", maxBooks = 7}
  (setName "Sam" (Person "Jack" 7))

testGetMaxBooks = (~=?)
  77
  (getMaxBooks (Person "Me" 77))

testSetMaxBooks = (~=?)
  Person {name = "Sam", maxBooks = 7}
  (setMaxBooks 7 (Person "Sam" 77))

testPersonToString = (~=?)
  "Tester (99 books)"
  (personToString (Person "Tester" 99))

runPersonTests = runTestTT $ TestList [ testMakePerson, testGetName
                                      , testSetName, testGetMaxBooks
                                      , testSetMaxBooks, testPersonToString ]
