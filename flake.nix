{
  description = "sydml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, self, nixpkgs, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib.compose;
        hpkgs = pkgs.haskell.packages.ghc98.extend (final: prev: {
          sydml = hlib.dontCheck (final.callCabal2nix "sydml" ./. {});
          # overly strict: base
          monadic-recursion-schemes = hlib.doJailbreak prev.monadic-recursion-schemes;
          # overly strict: base, bytestring, deepseq
          # qbe = hlib.doJailbreak prev.qbe;
          qbe = prev.callCabal2nix "qbe" ./vendor/qbe-hs {};
          # missing features, broken tests
          tree-sitter = prev.callCabal2nix "tree-sitter" ./vendor/haskell-tree-sitter/tree-sitter {};
          # broken tests
          dependent-hashmap = hlib.markUnbroken (hlib.dontCheck prev.dependent-hashmap);
          # out of date (we insist on using latest for the sole reason of
          # including the dual sperm operator, (<>:~).)
          lens = prev.lens_5_3_2;
          tree-sitter-sydml = import ./tree-sitter-sydml {
            inherit pkgs hpkgs system inputs;
          };
          # tree-sitter-sydml = prev.callCabal2nix "tree-sitter-sydml" ./tree-sitter-sydml {};
        });
      in rec {
        packages.default = (hpkgs.callCabal2nix "sydml" ./. {})
          .overrideAttrs (final: prev: {
            propagatedBuildInputs = with pkgs; [
              qbe
              gcc
            ] ++ prev.propagatedBuildInputs;
            nativeBuildInputs = [
              pkgs.tree-sitter
            ] ++ prev.nativeBuildInputs;
            doCheck = false; # HACK: for convenience of development.
          });

        checks.default = packages.default
          .overrideAttrs (final: prev: {
            doCheck = true;
          });

        checks.tree-sitter-sydml = hpkgs.tree-sitter-sydml
          .overrideAttrs (final: prev: {
            doCheck = true;
          });

        # checks.default = (hpkgs.callCabal2nix "sydml-test" ./. {})
        #   .overrideAttrs (final: prev: {
        #     nativeBuildInputs = [
        #       pkgs.tree-sitter
        #     ] ++ prev.nativeBuildInputs;
        #   });

        devShells.default = hpkgs.shellFor {
          packages = p: [
            p.sydml
            # p.presydc
          ];
          nativeBuildInputs = [
            hpkgs.cabal-fmt
            hpkgs.fourmolu
            hpkgs.haskell-language-server
            hpkgs.cabal-install
            hpkgs.hasktags
            pkgs.tree-sitter
            (hpkgs.calligraphy.overrideAttrs (final: prev: {
              propagatedBuildInputs = [
                pkgs.graphviz
              ] ++ prev.propagatedBuildInputs;
            }))
          ];
          propagatedBuildInputs = with pkgs; [
            qbe
            gcc
          ];
          withHoogle = true;
        };
      });
}
