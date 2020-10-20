# forfreeter

## Introduction
A simple Haskell library which allows to generate, and thus remove from the codebase, the boilerplatey helper instances for a given typeclass.
For now we are focusing on instances described here:
http://felixmulder.com/writing/2020/08/08/Revisiting-application-structure.html

## Example
Allows to write:
```haskell
class Monad m => Time m where
  currentTime :: m Int

mkOverlappable ''Time
```

Instead of:
```haskell
class Monad m => Time m where
  currentTime :: m Int

instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), Time m, MonadTrans t) => Time (t m) where
  currentTime = lift . currentTime
```
