{
  description = "sydml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
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
      });
    in {
      # inherit pkgs;
      packages.x86_64-linux = {
        default = hpkgs.shellFor {
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
          buildInputs = with pkgs; [
            qbe
            gcc
          ];
          withHoogle = true;
        };
      };
    };
}
