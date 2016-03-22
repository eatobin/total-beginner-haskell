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

runTests = runTestTT $ TestList [testMakePerson, testGetName, testSetName]
