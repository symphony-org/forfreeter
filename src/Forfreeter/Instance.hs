{-# LANGUAGE UndecidableInstances #-}
module Forfreeter.Instance where

import           Control.Monad             (join)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Language.Haskell.TH

mkOverlappable :: Name -> Q [Dec]
mkOverlappable cName = do
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
    -- function with single polymorphic parameter
    liftDecl (SigD name (ForallT [KindedTV paramName _] _ _)) = do
      let
        body = (NormalB $ AppE (VarE 'lift) (AppE (VarE name) (VarE paramName)))
      pure [FunD name [Clause [VarP paramName] body []]]
    -- function with single monomorphic parameter
    liftDecl (SigD name (AppT (AppT ArrowT _) (AppT _ _))) = do
      paramName <- newName "p1"
      let
        body = NormalB (InfixE (Just (VarE 'lift)) (VarE '($)) (Just (AppE (VarE name) (VarE paramName))))
      pure [FunD name [Clause [VarP paramName] body []]]
    -- function without parameters
    liftDecl (SigD name appT@(AppT _ _)) = do
      let
        body = NormalB $ AppE (VarE 'lift) (VarE name)
      pure [ValD (VarP name) body []]
    -- ignore everything else
    liftDecl _                  = pure []
  functionDeclarations <- fmap join (traverse liftDecl decs)
  pure [InstanceD overlappable ctx (AppT (ConT cName) instanceType) functionDeclarations]
