let
  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project2 =
            haskellPackagesNew.callPackage ./vsmt.nix {
              z3 = pkgs.z3;
              zlib = pkgs.zlib;
            };

          sbv = haskellPackagesNew.callPackage ./sbv.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  rec {
    vsmt = pkgs.haskellPackages.project2;
  }
