module Forfreeter.Instance where

import           Control.Monad.Trans.Class    (MonadTrans)
import           Data.Derive.TopDown.Instance (instance_)
import           Language.Haskell.TH

class Test a where
  foo :: a -> String

mkEmptyInstance :: Name -> Name -> Q [Dec]
mkEmptyInstance = instance_

mkOverlappable :: Name -> Name -> Q [Dec]
mkOverlappable cName tName = do
  [ InstanceD a b c d ] <- mkEmptyInstance cName tName
  ClassI cDec@(ClassD _ n _ _ foos ) _ <- reify cName
  [ sigD_@(SigD fName appT_) ] <- pure foos
  exp <- [e| \_ -> "Generated in overlappable" |]
  someFun <- pure $ FunD fName [ Clause [] (NormalB exp) []]
  pure $ [ InstanceD a b c [someFun] ]

mkShow :: Name -> Q [Dec]
mkShow name = [d|
  instance {-# OVERLAPS #-} Show $a where
    show _ = "hello from template show"
  |]
  where
    a = conT name
