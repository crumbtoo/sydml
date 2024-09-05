{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  hlib = pkgs.haskell.lib.compose;
  hs-pkgs = pkgs.haskell.packages.ghc98.override {
    # TODO: use vendored `tree-sitter` package.
    overrides = final: prev: {
      sydml = final.callCabal2nix "sydml" ./. { };
      # overly strict: `base`
      monadic-recursion-schemes = hlib.doJailbreak prev.monadic-recursion-schemes;
      # overly strict: `base`, `bytestring`, `deepseq`
      qbe = hlib.doJailbreak prev.qbe;
      # broken tests
      dependent-hashmap = hlib.dontCheck prev.dependent-hashmap;
    };
  };
in hs-pkgs.sydml
