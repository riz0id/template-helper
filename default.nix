{ ghc }:

let
  pkgs = import nix/pkgs.nix {
    inherit ghc;
  };
in pkgs.haskell.packages."${ghc}"