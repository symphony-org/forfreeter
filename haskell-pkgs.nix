let
  haskellNix = import (builtins.fetchTarball{
    url = "https://github.com/input-output-hk/haskell.nix/archive/6134e66e1decf2155ba500dd5de00adb06e2cdee.tar.gz";
  }) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
  import nixpkgsSrc nixpkgsArgs
