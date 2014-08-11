module Test.QuickCheck.Maybe (TestMaybe(..), runTestMaybe) where

import Data.Maybe

import Control.Alt
import Control.Alternative
import Control.MonadPlus
import Control.Plus

import Test.QuickCheck

newtype TestMaybe a = TestMaybe (Maybe a)

runTestMaybe :: forall a. TestMaybe a -> Maybe a
runTestMaybe (TestMaybe m) = m

lift :: forall a b. (Maybe a -> Maybe a -> b) -> TestMaybe a -> TestMaybe a -> b
lift f (TestMaybe x) (TestMaybe y) = f x y

instance arbMaybe :: (Arbitrary a) => Arbitrary (TestMaybe a) where
  arbitrary = TestMaybe <$> do
    b <- arbitrary
    if b then pure Nothing else Just <$> arbitrary

instance showTestMaybe :: (Show a, Show b) => Show (TestMaybe a) where
  show (TestMaybe x) = "TestMaybe " ++ show x

instance eqTestMaybe :: (Eq a) => Eq (TestMaybe a) where
  (==) = lift (==)
  (/=) = lift (/=)

instance ordTestMaybe :: (Ord a) => Ord (TestMaybe a) where
  compare = lift compare

instance functorTestMaybe :: Functor TestMaybe where
  (<$>) f (TestMaybe x) = TestMaybe $ f <$> x

instance applyTestMaybe :: Apply TestMaybe where
  (<*>) (TestMaybe f) (TestMaybe x) = TestMaybe $ f <*> x

instance applicativeTestMaybe :: Applicative TestMaybe where
  pure = TestMaybe <<< pure

instance altTestMaybe :: Alt TestMaybe where
  (<|>) (TestMaybe l) (TestMaybe r) = TestMaybe (l <|> r)

instance plusTestMaybe :: Plus TestMaybe where
  empty = TestMaybe empty

instance alternativeTestMaybe :: Alternative TestMaybe where

instance bindTestMaybe :: Bind TestMaybe where
  (>>=) (TestMaybe x) f = TestMaybe (x >>= runTestMaybe <<< f)

instance monadTestMaybe :: Monad TestMaybe

instance monadPlusTestMaybe :: MonadPlus TestMaybe
