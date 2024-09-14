{ pkgs ? import <nixpkgs> {}
, hpkgs ? pkgs.haskell.packages.ghc98
, ...
}@inputs:

(hpkgs.callCabal2nix "tree-sitter-sydml" ./. {})
  .overrideAttrs (final: prev: {
    nativeBuildInputs = [
      (pkgs.callPackage ./grammar.nix {})
    ] ++ prev.nativeBuildInputs;
  })
