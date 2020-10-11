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
    vm = varT m
    vt = varT t
  instanceType <- appT vt vm -- (t m)
  ctxType1 <- [t| Monad $(vm) |] -- Monad m
  ctxType2 <- [t| Monad $(appT vt vm) |] -- Monad (t m)
  ctxType3 <- [t| MonadTrans $(vt) |] -- MonadTrans t
  vTm <- varT m
  let
    ctxType4 = AppT (ConT cName) (vTm) -- Test m
    overlappable = Just Overlappable
    ctx = [ctxType1, ctxType2, ctxType3, ctxType4 ] -- TODO: (cName m) =>
    functionDeclarations = [] -- TODO: lift all existing functions
  pure [InstanceD overlappable ctx (AppT (ConT cName) instanceType) functionDeclarations]

mkShow :: Name -> Q [Dec]
mkShow name = [d|
  instance {-# OVERLAPS #-} Show $a where
    show _ = "hello from template show"
  |]
  where
    a = conT name
