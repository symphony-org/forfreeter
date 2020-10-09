module InstanceSpec where

import           Forfreeter.Instance
import           Hedgehog
import           Hedgehog.Gen        as Gen
import           Hedgehog.Range      as Range

data User = User
data User2 = User2

class SomeClass t where
  someFun :: t -> String
  default someFun :: Show t => t -> String
  someFun t = "Hello from someFun! Got: " <> show t

class SomeClass2 t where
  someFun2 :: t -> String

mkShow ''User
mkEmptyInstance ''SomeClass ''()
mkEmptyInstance ''SomeClass ''User

mkOverlappable ''SomeClass2 ''User2

prop_1 :: Property
prop_1 =
  property $ do
    ( show User ) === "hello from template show"

prop_some_class :: Property
prop_some_class =
  property $ do
    someFun User === "Hello from someFun! Got: hello from template show"

prop_some_class_2 :: Property
prop_some_class_2 =
  property $ do
    someFun2 User2 === "Generated in overlappable"

tests :: IO Bool
tests =
  checkParallel $$(discover)
