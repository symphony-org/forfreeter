module Forfreeter.Instance where

import           Control.Monad.Trans.Class    (MonadTrans)
import           Data.Derive.TopDown.Instance (instance_)
import           Language.Haskell.TH

mkEmptyInstance :: Name -> Name -> Q [Dec]
mkEmptyInstance cName tName = instance_ cName tName

mkOverlappable :: Name -> Name -> Q [Dec]
mkOverlappable _ _ = undefined

mkShow :: Name -> Q [Dec]
mkShow name = [d|
  instance {-# OVERLAPS #-} Show $a where
    show _ = "hello from template show"
  |]
  where
    a = conT name
