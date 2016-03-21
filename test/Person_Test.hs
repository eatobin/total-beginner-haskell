module Person_Test where

import Person
import Test.HUnit

testGetName = TestCase $ assertEqual
  "Should return name = Jack" "Jack"
  (getName (Person "Jack" 7))

testSetName = TestCase $ assertEqual
  "Should return name = Person {name = 'Bob', maxBooks = 5}"
  Person {name = "Sam", maxBooks = 7}
  (setName "Sam" (Person "Jack" 7))

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1,2))

main7 = runTestTT test1



main12 = runTestTT testGetName
main99 = runTestTT $ TestList [testSetName, testGetName]
