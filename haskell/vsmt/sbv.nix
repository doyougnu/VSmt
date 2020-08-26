{ mkDerivation, array, async, base, bytestring, containers
, crackNum, deepseq, directory, doctest, filepath, gauge
, generic-deriving, Glob, hlint, mtl, pretty, process, QuickCheck
, random, silently, stdenv, syb, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, template-haskell, time, transformers, z3
}:
mkDerivation {
  pname = "sbv";
  version = "8.7";
  sha256 = "34107492915af8a3577394ced6c2cb8388a23629e512edba740646a0f2a03746";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array async base containers crackNum deepseq directory filepath
    generic-deriving mtl pretty process QuickCheck random syb
    template-haskell time transformers
  ];
  testHaskellDepends = [
    base bytestring containers crackNum directory doctest filepath Glob
    hlint mtl QuickCheck random syb tasty tasty-golden tasty-hunit
    tasty-quickcheck template-haskell
  ];
  testSystemDepends = [ z3 ];
  benchmarkHaskellDepends = [
    base containers crackNum deepseq directory filepath gauge mtl
    process random silently syb
  ];
  homepage = "http://leventerkok.github.com/sbv/";
  description = "SMT Based Verification: Symbolic Haskell theorem prover using SMT solving";
  license = stdenv.lib.licenses.bsd3;
}
