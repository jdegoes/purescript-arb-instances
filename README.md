# Module Documentation

## Module Test.QuickCheck.Either

### Types

    data TestEither a b where
      TestEither :: Either a b -> TestEither a b


### Type Class Instances

    instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (TestEither a b)


### Values

    runTestEither :: forall a b. TestEither a b -> Either a b


## Module Test.QuickCheck.Maybe

### Types

    data TestMaybe a where
      TestMaybe :: Maybe a -> TestMaybe a


### Type Class Instances

    instance arbMaybe :: (Arbitrary a) => Arbitrary (TestMaybe a)


### Values

    runTestMaybe :: forall a. TestMaybe a -> Maybe a


## Module Test.QuickCheck.Tuple

### Types

    data TestTuple a b where
      TestTuple :: Tuple a b -> TestTuple a b


### Type Class Instances

    instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (TestTuple a b)


### Values

    runTestTuple :: forall a b. TestTuple a b -> Tuple a b



