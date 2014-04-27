module Test.QuickCheck.Maybe where

import Data.Maybe

import Test.QuickCheck

data TestMaybe a = TestMaybe (Maybe a)

runTestMaybe :: forall a. TestMaybe a -> Maybe a
runTestMaybe (TestMaybe m) = m

instance arbMaybe :: (Arbitrary a) => Arbitrary (TestMaybe a) where
  arbitrary = TestMaybe <$> do
    b <- arbitrary
    if b then pure Nothing else Just <$> arbitrary
