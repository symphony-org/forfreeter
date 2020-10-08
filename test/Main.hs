module Main where

import           Hedgehog.Main (defaultMain)
import           InstanceSpec  as InstanceSpec

main :: IO ()
main = defaultMain [ InstanceSpec.tests ]
