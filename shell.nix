
with (import <nixpkgs> { });

let
  pkg = pkgs.haskell.packages.ghc962.callCabal2nix "language-haskell-extract" ./. { };
in pkg.env