module Test.QuickCheck.Tuple (TestTuple(..), runTestTuple) where

import Data.Tuple
import Data.Monoid

import Test.QuickCheck

data TestTuple a b = TestTuple (Tuple a b)

runTestTuple :: forall a b. TestTuple a b -> Tuple a b
runTestTuple (TestTuple t) = t

lift :: forall a b c. (Tuple a b -> Tuple a b -> c) -> TestTuple a b -> TestTuple a b -> c
lift f (TestTuple x) (TestTuple y) = f x y

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (TestTuple a b) where
  arbitrary = TestTuple <$> (Tuple <$> arbitrary <*> arbitrary)
    
instance showTestTuple :: (Show a, Show b) => Show (TestTuple a b) where
  show (TestTuple x) = "TestTuple " ++ show x
    
instance eqTestTuple :: (Eq a, Eq b) => Eq (TestTuple a b) where
  (==) = lift (==)
  (/=) = lift (/=)
  
instance ordTestTuple :: (Ord a, Ord b) => Ord (TestTuple a b) where
  compare = lift compare

instance functorTestTuple :: Functor (TestTuple a) where
  (<$>) f (TestTuple x) = TestTuple $ f <$> x
  
instance applyTestTuple :: (Semigroup a) => Apply (TestTuple a) where
  (<*>) (TestTuple f) (TestTuple x) = TestTuple $ f <*> x
  
instance applicativeTestTuple :: (Monoid a) => Applicative (TestTuple a) where
  pure = TestTuple <<< pure
  
instance bindTestTuple :: (Semigroup a) => Bind (TestTuple a) where
  (>>=) (TestTuple x) f = TestTuple (x >>= runTestTuple <<< f)

instance monadTestTuple :: (Monoid a) => Monad (TestTuple a)
