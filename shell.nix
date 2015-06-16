let
  pkgs = import <nixpkgs> { };
  /* inherit (pkgs.haskell.lib) withHoogle; */

  ghc = with pkgs.haskell-ng.lib; pkgs.haskell-ng.packages.ghc7101.override {
    overrides = self: super: {
      hdevtools = overrideCabal super.hdevtools (drv: {
        broken = false;
        src = pkgs.fetchFromGitHub {
          owner = "schell";
          repo = "hdevtools";
          rev = "5a946486bb716a34e923cdd47338488c54cb1ba5";
          sha256 = "0vba29dqbr2gjsgv7q11ygw48k1fadv9qrrvpa9rcrm67mgxra1a";
        };
      });

      ghc-mod = overrideCabal super.ghc-mod (drv: {
        broken = false;
        src = pkgs.fetchFromGitHub {
          owner = "DanielG";
          repo = "ghc-mod";
          rev = "1e381a12a9532eb9d7f38051674d210af7ce1ab7";
          sha256 = "1f59bfisvyb0c5i507p60436syjidcnrbylfhr2lwfi6ajb698q6";
        };
        buildDepends = drv.buildDepends ++ [ self.cabal-helper self.cereal ];
      });
    };
  };

  ghcPackages = ghc.ghcWithPackages (p: with p; [
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
    /* (withHoogle ghcPackages) */

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
