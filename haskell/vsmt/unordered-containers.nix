{ mkDerivation, base, bytestring, ChasingBottoms, containers
, deepseq, gauge, hashable, hashmap, HUnit, mtl, QuickCheck, random
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "unordered-containers";
  version = "0.2.12.0";
  sha256 = "481dab4ea59f10feaa7d65ecb8ae5d136d130bdbacf2356d414b3926d638bd20";
  libraryHaskellDepends = [ base deepseq hashable ];
  testHaskellDepends = [
    base ChasingBottoms containers hashable HUnit QuickCheck random
    test-framework test-framework-hunit test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [
    base bytestring containers deepseq gauge hashable hashmap mtl
    random
  ];
  homepage = "https://github.com/haskell-unordered-containers/unordered-containers";
  description = "Efficient hashing-based container types";
  license = stdenv.lib.licenses.bsd3;
}
