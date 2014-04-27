module Test.QuickCheck.Tuple where

import Data.Tuple

import Test.QuickCheck

data TestTuple a b = TestTuple (Tuple a b)

runTestTuple :: forall a b. TestTuple a b -> Tuple a b
runTestTuple (TestTuple t) = t

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (TestTuple a b) where
arbitrary = TestTuple <$> (Tuple <$> arbitrary <*> arbitrary)
