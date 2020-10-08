module InstanceSpec where

import           Hedgehog
import           Hedgehog.Gen   as Gen
import           Hedgehog.Range as Range
import           Instance

data User = User

mkShow ''User

prop_1 :: Property
prop_1 =
  property $ do
    ( show User ) === "hello from template show"

tests :: IO Bool
tests =
  checkParallel $$(discover)
