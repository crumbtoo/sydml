{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  hlib = pkgs.haskell.lib.compose;
  hs-pkgs = pkgs.haskell.packages.ghc98
    .override {
      overrides = final: prev: {
        qbe = hlib.doJailbreak prev.qbe;
      };
    };
in
hs-pkgs.callPackage (hs-pkgs.callCabal2nix "sydml" (pkgs.lib.cleanSource ./.) { }) {}
