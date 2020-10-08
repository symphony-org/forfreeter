{ pkgs ? import ./haskell-pkgs.nix
}:

let
  hsPkgs = import ./. { inherit pkgs; };
in
  hsPkgs.shellFor {
    tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; };

    buildInputs = with pkgs.haskellPackages;
      [ ghcid ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
