module InstanceSpec where

import           Forfreeter.Instance
import           Hedgehog
import           Hedgehog.Gen        as Gen
import           Hedgehog.Range      as Range

data User = User

class SomeClass t where
  someFun :: t -> String
  default someFun :: Show t => t -> String
  someFun t = "Hello from someFun! Got: " <> show t

mkShow ''User
mkEmptyInstance ''SomeClass ''()

prop_1 :: Property
prop_1 =
  property $ do
    ( show User ) === "hello from template show"

prop_some_class :: Property
prop_some_class =
  property $ do
    someFun () === "Hello from someFun! Got: ()"

tests :: IO Bool
tests =
  checkParallel $$(discover)
