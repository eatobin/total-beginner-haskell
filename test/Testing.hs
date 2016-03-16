module Testing where

import Test.QuickCheck

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (ys++xs) == reverse xs ++ reverse ys
-- prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

mainly = quickCheck prop_revapp
