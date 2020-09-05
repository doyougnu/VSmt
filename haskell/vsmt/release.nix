{ compiler ? "ghc884" }:

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

              sbv = haskellPackagesNew.callPackage ./sbv.nix { };
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
                                 haskellPackages.hindent
                                 haskellPackages.ghcide
                                 zlib
                                 z3
                                 cabal-install
                               ];
    };
  }

# extraCmds = ''
# export LD_LIBRARY_PATH+=:${self.zlib}/lib
# '';
