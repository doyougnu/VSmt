{ mkDerivation, base, bifunctors, deepseq, gauge, guage, mtl
, QuickCheck, sbv, silently, stdenv, tasty, tasty-golden
, tasty-hunit, tasty-quickcheck, text, unordered-containers
}:
mkDerivation {
  pname = "vsmt";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors deepseq mtl sbv text unordered-containers
  ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-golden tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base deepseq gauge guage silently ];
  homepage = "https://github.com/doyougnu/VSmt";
  description = "SMT Based Verification with first class variational programming capabilities";
  license = stdenv.lib.licenses.bsd3;
}
