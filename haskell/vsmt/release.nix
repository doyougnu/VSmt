{ compiler ? "ghc8102" }:

let
  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages.${compiler}.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              vsmt = haskellPackagesNew.callPackage ./vsmt.nix {
                z3 = pkgs.z3;
                zlib = pkgs.zlib;
              };

              # sbv = haskellPackagesNew.callPackage ./sbv.nix {
              #   z3 = pkgs.z3;
              # };

              sbv = haskellPackagesNew.callPackage ~/programming/sbv/sbv.nix { };
              unordered-containers = haskellPackagesNew.callPackage ./unordered-containers.nix {};
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  rec {
    vsmt = pkgs.haskell.packages.${compiler}.vsmt;

    vsmt-shell = pkgs.haskell.packages.${compiler}.shellFor {
      packages = p: [vsmt];
      withIDe = true;
      buildInputs = with pkgs; [ haskellPackages.hlint
                                 haskellPackages.stylish-haskell
                                 haskellPackages.hasktags
                                 haskellPackages.apply-refact
                                 # haskellPackages.hindent
                                 haskellPackages.ghcide
                                 haskellPackages.ghc-prof-flamegraph
                                 haskellPackages.profiteur
                                 zlib
                                 z3
                                 cabal-install
                               ];
    };
  }

# extraCmds = ''
# export LD_LIBRARY_PATH+=:${self.zlib}/lib
# '';
