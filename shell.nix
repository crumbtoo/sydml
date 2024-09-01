{ system ? builtins.currentSystem, devTools ? true }:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  hs-pkgs = pkgs.haskell.packages.ghc98
    .extend (final: prev: {
        sydml = import ./sydml.nix { inherit system; };
    });

in hs-pkgs.shellFor {
  packages = p: [ # p.sydml
                ];
  buildInputs = with pkgs;
    [ qbe
    ];
  nativeBuildInputs = with pkgs;
    [ ghc
      cabal-install
      tree-sitter
      nodejs-slim # for tree-sitter
    ]
    ++ lib.optional devTools
      [ niv
        hlint
        fourmolu
        cabal-fmt
        (haskell-language-server.override { supportedGhcVersions = [ "98" ]; })
        # (ghc.withPackages (p: [ p.haskell-language-server.override { supportedGhcVersions = [ "98" ]; } ]))
        # hs-pkgs.haskell-language-server
        # (ghc.withPackages (p: [ p.haskell-language-server.override  ]))
      ];
}
