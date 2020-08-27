# (import ./release.nix {}).vsmt-shell
{ pkgs ? import <nixpkgs> {}, compiler ? "ghc844"}:

let
  vsmt = (import ./release.nix {}).vsmt;
  # extradeps = import ./extradeps.nix pkgs;
  # foreign = builtins.attrValues extradeps.foreign;
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskell.packages.${compiler}.ghcWithHoogle (h: [ vsmt zlib z3 ]))
  ];
}
