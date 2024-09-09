{
  description = "sydml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, self, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib.compose;
        hpkgs = pkgs.haskell.packages.ghc98.extend (final: prev: {
          sydml = final.callCabal2nix "sydml" ./. {};
          # presydc = final.callCabal2nix "presydc" ./presydc {};
          # overly strict: base
          monadic-recursion-schemes = hlib.doJailbreak prev.monadic-recursion-schemes;
          # overly strict: base, bytestring, deepseq
          # qbe = hlib.doJailbreak prev.qbe;
          qbe = prev.callCabal2nix "qbe" ./vendor/qbe-hs {};
          # broken tests
          dependent-hashmap = hlib.markUnbroken (hlib.dontCheck prev.dependent-hashmap);
          # out of date (we insist on using latest for the sole reason of
          # including the dual sperm operator, (<>:~).)
          lens = prev.lens_5_3_2;
        });
      in {
        inherit hpkgs;
        # defaultPackage = hpkgs.callCabal2nix "sydml" ./. {};
        defaultPackage = (hpkgs.callCabal2nix "sydml" ./. {})
          .overrideAttrs (final: prev: {
            propagatedBuildInputs = with pkgs; [
              qbe
              gcc
            ]
            ++ prev.propagatedBuildInputs;
          });

        devShell = hpkgs.shellFor {
          packages = p: [
            p.sydml
            # p.presydc
          ];
          nativeBuildInputs = [
            hpkgs.cabal-fmt
            hpkgs.fourmolu
            hpkgs.haskell-language-server
            hpkgs.cabal-install
          ];
          propagatedBuildInputs = with pkgs; [
            qbe
            gcc
          ];
          withHoogle = true;
        };
      });
}
