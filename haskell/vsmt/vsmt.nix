{ mkDerivation, base, bifunctors, containers, deepseq, gauge
, monad-logger, mtl, QuickCheck, sbv, silently, stdenv, tasty
, tasty-golden, tasty-hunit, tasty-quickcheck, text, transformers
, unordered-containers, z3, zlib
}:
mkDerivation {
  pname = "vsmt";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers deepseq monad-logger mtl sbv text
    transformers unordered-containers
  ];
  librarySystemDepends = [ z3 zlib ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-golden tasty-hunit tasty-quickcheck
  ];
  executableSystemDepends = [ z3 zlib ];
  benchmarkHaskellDepends = [ base deepseq gauge silently ];
  homepage = "https://github.com/doyougnu/VSmt";
  description = "SMT Based Verification with first class variational programming capabilities";
  license = stdenv.lib.licenses.bsd3;
}
