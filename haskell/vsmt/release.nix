{ compiler ? "ghc8103" }:

let
  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages.${compiler}.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              vsmt = haskellPackagesNew.callPackage ./vsmt.nix {
                z3-haskell = haskellPackagesNew.callPackage /home/doyougnu/programming/haskell-z3/z3.nix { z3 = pkgs.z3; };
                zlib = pkgs.zlib;
              };

              # sbv = haskellPackagesNew.callPackage ./sbv.nix {
              #   z3 = pkgs.z3;
              # };

              # sbv = haskellPackagesNew.callPackage ./lib/sbv/sbv.nix { };
              # sbv = haskellPackagesNew.callPackage ./sbv-git.nix { };
              # sbv = haskellPackagesNew.callPackage ~/Programming/sbv/sbv.nix { };
              unordered-containers = haskellPackagesNew.callPackage ./unordered-containers.nix {};
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
  unstable = import <unstable> { };

in
  rec {
    vsmt = pkgs.haskell.packages.${compiler}.vsmt;

    vsmt-shell = pkgs.haskell.packages.${compiler}.shellFor {
      packages = p: [vsmt];
      withIDe = true;
      buildInputs = [ pkgs.haskellPackages.hlint
                      pkgs.haskellPackages.stylish-haskell
                      pkgs.haskellPackages.hasktags
                      pkgs.haskellPackages.apply-refact
                      # pkgs.haskellPackages.hindent
                      pkgs.haskellPackages.ghcide
                      pkgs.haskellPackages.ghc-prof-flamegraph
                      pkgs.haskellPackages.profiteur
                      pkgs.zlib
                      pkgs.z3
                      pkgs.cabal-install
                      # unstable.julia
                    ];
    };
  }

# extraCmds = ''
# export LD_LIBRARY_PATH+=:${self.zlib}/lib
# '';
