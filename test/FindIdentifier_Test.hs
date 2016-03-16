-- file FindIdentifier_Test.hs
module FindIdentifier_Test where

import FindIdentifier( findIdentifier )
import Test.HUnit

testEmpty = TestCase $ assertEqual
  "Should get Nothing from an empty string" Nothing ( findIdentifier "" (1, 1) )

main9 = runTestTT testEmpty

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1,2))

main7 = runTestTT test1
