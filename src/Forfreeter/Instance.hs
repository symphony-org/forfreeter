{-# LANGUAGE UndecidableInstances #-}
module Forfreeter.Instance where

import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Language.Haskell.TH

{-
instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), Test m, MonadTrans t) => Test (t m) where
  foo a = lift $ foo2 a
-}

mkEmptyInstance :: Name -> Q [Dec]
mkEmptyInstance cName = do
  m <- newName "m"
  t <- newName "t"
  let
    vT = varT t
  instanceType <- appT vT (varT m) -- (t m)
  ctxType1 <- [t| Monad $(varT m) |] -- Monad m
  ctxType2 <- [t| Monad $(appT vT (varT m)) |] -- Monad (t m)
  let
    overlappable = Nothing
    ctx = [ctxType1, ctxType2] -- TODO: (cName m, MonadTrans t) =>
    functionDeclarations = [] -- TODO: lift all existing functions
  pure [InstanceD overlappable ctx (AppT (ConT cName) instanceType) functionDeclarations]

mkShow :: Name -> Q [Dec]
mkShow name = [d|
  instance {-# OVERLAPS #-} Show $a where
    show _ = "hello from template show"
  |]
  where
    a = conT name
