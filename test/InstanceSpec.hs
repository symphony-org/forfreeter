{-# LANGUAGE UndecidableInstances #-}
module InstanceSpec where

import           Forfreeter.Instance
import           Hedgehog            hiding (Test)
import           Hedgehog.Gen        as Gen
import           Hedgehog.Range      as Range

data User = User

class Test m where
  foo :: a -> m String

instance Test IO where
  foo _ = pure "Hi from Test.foo"

mkShow ''User

mkEmptyInstance ''Test

prop_1 :: Property
prop_1 =
  property $ do
    ( show User ) === "hello from template show"

prop_some_class :: Property
prop_some_class =
  property $ do
    res <- foo ()
    res === "Hi from Test.foo"

tests :: IO Bool
tests =
  checkParallel $$(discover)
