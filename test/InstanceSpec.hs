{-# LANGUAGE UndecidableInstances #-}
module InstanceSpec where

import           Control.Monad.Trans.Class    (MonadTrans)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Function                ((&))
import           Forfreeter.Instance
import           Hedgehog                     hiding (Test)
import           Hedgehog.Gen                 as Gen
import           Hedgehog.Range               as Range

---- Queue

class Monad m => Queue m where
  send :: a -> m ()

mkEmptyInstance ''Queue

newtype NoOpQueue m a = NoOpQueue
  { runNoOpQueue :: m a
  }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadTrans via IdentityT

instance Monad m => Queue (NoOpQueue m) where
  send _ = pure ()

---- Repository

class Monad m => Repository m where
  save :: a -> m ()

mkEmptyInstance ''Repository

newtype NoOpRepository m a = NoOpRepository
  { runNoOpRepository :: m a
  }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadTrans via IdentityT

instance Monad m => Repository (NoOpRepository m) where
  save _ = pure ()

----- program

program ::
     Monad m
  => Queue m
  => Repository m
  => m Bool
program = do
  send "value"
  save "value"
  pure True

tests :: IO Bool
tests = program
  & runNoOpQueue
  & runNoOpRepository
