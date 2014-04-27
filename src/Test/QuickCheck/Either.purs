module Test.QuickCheck.Either (TestEither(..), runTestEither) where

import Data.Either

import Test.QuickCheck

data TestEither a b = TestEither (Either a b)

runTestEither :: forall a b. TestEither a b -> Either a b
runTestEither (TestEither e) = e

lift :: forall a b c. (Either a b -> Either a b -> c) -> TestEither a b -> TestEither a b -> c
lift f (TestEither x) (TestEither y) = f x y

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (TestEither a b) where
  arbitrary = TestEither <$> do
    b <- arbitrary
    if b then Left <$> arbitrary else Right <$> arbitrary
    
instance showTestEither :: (Show a, Show b) => Show (TestEither a b) where
  show (TestEither x) = "TestEither " ++ show x
    
instance eqTestEither :: (Eq a, Eq b) => Eq (TestEither a b) where
  (==) = lift (==)
  (/=) = lift (/=)
  
instance ordTestEither :: (Ord a, Ord b) => Ord (TestEither a b) where
  compare = lift compare

instance functorTestEither :: Functor (TestEither a) where
  (<$>) f (TestEither x) = TestEither $ f <$> x
  
instance applyTestEither :: Apply (TestEither a) where
  (<*>) (TestEither f) (TestEither x) = TestEither $ f <*> x
  
instance applicativeTestEither :: Applicative (TestEither a) where
  pure = TestEither <<< pure
  
instance bindTestEither :: Bind (TestEither a) where
  (>>=) (TestEither x) f = TestEither (x >>= runTestEither <<< f)

instance monadTestEither :: Monad (TestEither a)
