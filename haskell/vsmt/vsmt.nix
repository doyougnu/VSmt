{ mkDerivation, base, bifunctors, containers, deepseq, gauge
, hashable, monad-logger, mtl, QuickCheck, sbv, silently, stdenv
, tasty, tasty-golden, tasty-quickcheck, text, filepath, bytestring
, transformers, unordered-containers, z3, zlib, tasty-hspec
}:
mkDerivation {
  pname = "vsmt";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers deepseq hashable monad-logger mtl sbv
    text transformers unordered-containers
  ];
  librarySystemDepends = [ z3 zlib ];
  executableSystemDepends = [ z3 zlib ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-golden tasty-quickcheck tasty-hspec filepath
    bytestring
  ];
  benchmarkHaskellDepends = [ base deepseq gauge silently ];
  homepage = "https://github.com/doyougnu/VSmt";
  description = "SMT Based Verification with first class variational programming capabilities";
  license = stdenv.lib.licenses.bsd3;
}