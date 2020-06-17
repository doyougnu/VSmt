let
  pkgs = import <nixpkgs> {};
  inherit (pkgs) haskellPackages;
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.zlib
    pkgs.z3
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.stylish-haskell
    pkgs.haskellPackages.hasktags
    pkgs.haskellPackages.apply-refact
    pkgs.haskellPackages.hindent
  ];
}
