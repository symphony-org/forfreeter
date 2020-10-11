{-# LANGUAGE UndecidableInstances #-}
module Forfreeter.Instance where

import           Control.Monad             (join)
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
    ctx = [ctxType1, ctxType2, ctxType3, ctxType4 ]
  (ClassI (ClassD _cxt _name _tyVarBndr _funDep decs) _knownInstances) <- reify cName
  let
    liftDecl :: Dec -> Q [Dec]
    liftDecl (SigD name (ForallT [KindedTV paramName _] _ _)) = do
      pure [FunD name [Clause [VarP paramName] body []]]
      where
        -- TODO: implement lift for more than one parameter
        body = (NormalB $ AppE (VarE 'lift) (AppE (VarE name) (VarE paramName)))
    liftDecl _                  = pure []
  functionDeclarations <- fmap join (traverse liftDecl decs)
  pure [InstanceD overlappable ctx (AppT (ConT cName) instanceType) functionDeclarations]

mkShow :: Name -> Q [Dec]
mkShow name = [d|
  instance {-# OVERLAPS #-} Show $a where
    show _ = "hello from template show"
  |]
  where
    a = conT name
