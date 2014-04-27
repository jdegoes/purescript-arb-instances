module Test.QuickCheck.Either where

import Data.Either

import Test.QuickCheck

data TestEither a b = TestEither (Either a b)

runTestEither :: forall a b. TestEither a b -> Either a b
runTestEither (TestEither e) = e

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (TestEither a b) where
  arbitrary = TestEither <$> do
    b <- arbitrary
    if b then Left <$> arbitrary else Right <$> arbitrary

