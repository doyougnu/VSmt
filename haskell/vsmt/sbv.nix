{ mkDerivation, array, async, base, bench-show, bytestring
, containers, crackNum, deepseq, directory, doctest, filepath
, gauge, Glob, hlint, mtl, pretty, process, QuickCheck, random
, silently, stdenv, syb, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, template-haskell, time, transformers, z3
, abc-verifier uniplate
}:
mkDerivation {
  pname = "sbv";
  version = "8.8";
  sha256 = "feed7f80b1a073ccccc86889daacf43a138c98e11e5aad979468c03ed32ba076";
  doCheck = false;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array async base containers crackNum deepseq directory filepath mtl
    pretty process QuickCheck random syb template-haskell time
    transformers uniplate
  ];
  testHaskellDepends = [
    base bytestring containers crackNum directory doctest filepath Glob
    hlint mtl QuickCheck random tasty tasty-golden tasty-hunit
    tasty-quickcheck
  ];
  testSystemDepends = [ z3 abc-verifier ];
  benchmarkHaskellDepends = [
    base bench-show containers crackNum deepseq directory filepath
    gauge mtl process random silently syb time
  ];
  homepage = "http://leventerkok.github.com/sbv/";
  description = "SMT Based Verification: Symbolic Haskell theorem prover using SMT solving";
  license = stdenv.lib.licenses.bsd3;
}
