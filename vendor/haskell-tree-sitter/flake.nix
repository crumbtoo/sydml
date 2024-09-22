{
  description = "haskell-tree-sitter";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, self, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib.compose;
        hpkgs = pkgs.haskell.packages.ghc98.extend (final: prev: {
          tree-sitter = final.callCabal2nix "tree-sitter" ./tree-sitter {};
        });
      in {
        packages.default = hpkgs.tree-sitter;

        devShells.default = hpkgs.shellFor {
          packages = p: [
            p.tree-sitter
          ];
          nativeBuildInputs = [
            hpkgs.cabal-fmt
            hpkgs.fourmolu
            hpkgs.haskell-language-server
            hpkgs.cabal-install
            hpkgs.hasktags
            pkgs.tree-sitter
          ];
          propagatedBuildInputs = with pkgs; [
            qbe
            gcc
          ];
          withHoogle = true;
        };
      });
}
