let
  pkgs = import <nixpkgs> { };
  inherit (pkgs.haskell.lib) withHoogle;

  ghc = pkgs.haskellPackages;
  ghcPackages = ghc.ghcWithHoogle (p: with p; [
    ipprint
    ghc-mod
    hdevtools
    stylish-haskell
    cabal-install
    pretty-show
    cabal2nix
    ghcid

    split
    aeson
    text
    vector
    hashmap
    unordered-containers
    haskell-generate
    haskell-src-exts
    haskell-src-exts-qq

    hspec
  ]);

in

with pkgs;

runCommand "dummy" {
  buildInputs = [
    ghcPackages

    python27Full
    pythonPackages.pycapnp
    capnproto
  ];
  shellHook = ''
    export NIX_GHC="${ghcPackages}/bin/ghc"
    export NIX_GHCPKG="${ghcPackages}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${ghcPackages}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
} ""
