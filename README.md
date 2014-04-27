# Module Documentation

## Module Test.QuickCheck.Either

### Types

    data TestEither a b where


### Type Class Instances

    instance applicativeTestEither :: Applicative (TestEither a)

    instance applyTestEither :: Apply (TestEither a)

    instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (TestEither a b)

    instance bindTestEither :: Bind (TestEither a)

    instance eqTestEither :: (Eq a, Eq b) => Eq (TestEither a b)

    instance functorTestEither :: Functor (TestEither a)

    instance monadTestEither :: Monad (TestEither a)

    instance ordTestEither :: (Ord a, Ord b) => Ord (TestEither a b)

    instance showTestEither :: (Show a, Show b) => Show (TestEither a b)


### Values

    runTestEither :: forall a b. TestEither a b -> Either a b


## Module Test.QuickCheck.Maybe

### Types

    data TestMaybe a where


### Type Class Instances

    instance alternativeTestMaybe :: Alternative TestMaybe

    instance applicativeTestMaybe :: Applicative TestMaybe

    instance applyTestMaybe :: Apply TestMaybe

    instance arbMaybe :: (Arbitrary a) => Arbitrary (TestMaybe a)

    instance bindTestMaybe :: Bind TestMaybe

    instance eqTestMaybe :: (Eq a) => Eq (TestMaybe a)

    instance functorTestMaybe :: Functor TestMaybe

    instance monadTestMaybe :: Monad TestMaybe

    instance ordTestMaybe :: (Ord a) => Ord (TestMaybe a)

    instance showTestMaybe :: (Show a, Show b) => Show (TestMaybe a)


### Values

    runTestMaybe :: forall a. TestMaybe a -> Maybe a


## Module Test.QuickCheck.Tuple

### Types

    data TestTuple a b where


### Type Class Instances

    instance applicativeTestTuple :: (Monoid a) => Applicative (TestTuple a)

    instance applyTestTuple :: (Semigroup a) => Apply (TestTuple a)

    instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (TestTuple a b)

    instance bindTestTuple :: (Semigroup a) => Bind (TestTuple a)

    instance eqTestTuple :: (Eq a, Eq b) => Eq (TestTuple a b)

    instance functorTestTuple :: Functor (TestTuple a)

    instance monadTestTuple :: (Monoid a) => Monad (TestTuple a)

    instance ordTestTuple :: (Ord a, Ord b) => Ord (TestTuple a b)

    instance showTestTuple :: (Show a, Show b) => Show (TestTuple a b)


### Values

    runTestTuple :: forall a b. TestTuple a b -> Tuple a b