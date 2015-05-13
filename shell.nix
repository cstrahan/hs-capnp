let
  pkgs = import <nixpkgs> { };

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
          rev = "247e4e0e7616fe1fecc68fdcf80d6249ac4cee4f";
          sha256 = "02i6z0navp5a73nk9k46rh01hl5r10s6gzq1c6fmcrdrzjq6nwv4";
        };
        buildDepends = drv.buildDepends ++ [ self.cabal-helper self.cereal ];
      });
    };
  };

  withHoogle = haskellEnv:
    ghc.callPackage <nixpkgs/pkgs/development/libraries/haskell/hoogle/local.nix> {
      packages = haskellEnv.paths;
    };

  ghcPackages = ghc.ghcWithPackages (p: with p; [
    ipprint
    ghc-mod
    hdevtools
    stylish-haskell
    cabal-install
    pretty-show
    cabal2nix

    aeson
    text
    vector
    hashmap
    unordered-containers

    hspec
  ]);

in

with pkgs;

runCommand "dummy" {
  buildInputs = [
    ghcPackages
    (withHoogle ghcPackages)

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
