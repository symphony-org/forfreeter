## Forfreeter

#### Development
Enter `cabal new-repl forfreeter` in the nix shell. Then in order to check the generated code use:
```
import Language.Haskell.TH.Lib

class SomeClass a where

$(stringE . show =<< mkOverlappable ''SomeClass)
```
