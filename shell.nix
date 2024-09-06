{ pkgs ? import <nixpkgs> { } }:

let
  hpkgs = pkgs.haskell.packages.ghc98;
in
hpkgs.shellFor {
  packages = p: [
    # (p.callCabal2nix ./sydml.cabal {})
  ];

  nativeBuildInputs = with pkgs; [
    cabal2nix
  ];
}
