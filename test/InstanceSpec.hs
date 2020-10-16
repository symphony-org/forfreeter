{-# LANGUAGE UndecidableInstances #-}
module InstanceSpec where

import           Control.Monad.Trans.Class    (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT(..), ask)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Function                ((&))
import           Forfreeter.Instance
import           Hedgehog                     hiding (Test)
import           Hedgehog.Gen                 as Gen
import           Hedgehog.Range               as Range
import           Language.Haskell.TH

---- Time

class Monad m => Time m where
  currentTime :: m Int

mkOverlappable ''Time
mkConst ''Time

runConstTime :: Int -> ConstTime m a -> m a
runConstTime time = flip runReaderT time . unConstTime

instance Monad m => Time (ConstTime m) where
  currentTime = ConstTime ask

---- Queue

class Monad m => Queue m where
  send :: a -> m ()
  send2 :: String -> m ()

mkOverlappable ''Queue

newtype NoOpQueue m a = NoOpQueue
  { runNoOpQueue :: m a
  }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadTrans via IdentityT

instance Monad m => Queue (NoOpQueue m) where
  send _ = pure ()
  send2 _ = pure ()

---- Repository

class Monad m => Repository m where
  save :: a -> m ()
  deleteAll :: m ()

mkOverlappable ''Repository

newtype NoOpRepository m a = NoOpRepository
  { runNoOpRepository :: m a
  }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadTrans via IdentityT

instance Monad m => Repository (NoOpRepository m) where
  save _ = pure ()
  deleteAll = pure ()

----- program

program ::
     Monad m
  => Time m
  => Queue m
  => Repository m
  => m Bool
program = do
  time <- currentTime
  send "value"
  send2 "value"
  save "value"
  pure True

tests :: IO Bool
tests = program
  & runNoOpQueue
  & runConstTime 10
  & runNoOpRepository
