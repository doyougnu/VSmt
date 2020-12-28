{ mkDerivation, array, async, base, bench-show, bytestring
, containers, crackNum, deepseq, directory, doctest, fetchgit
, filepath, gauge, Glob, hlint, mtl, pretty, process, QuickCheck
, random, silently, stdenv, syb, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, template-haskell, text, time, transformers
, uniplate, z3, abc-verifier
}:
mkDerivation {
  pname = "sbv";
  version = "8.9.5";
  src = fetchgit {
    url = "https://github.com/doyougnu/sbv.git";
    sha256 = "0n5sx2b0dk4wnrkm6qgr4sy45yz21bnv86j2wc24pfpm23n0i58l";
    rev = "4acd0fc6d2ddaae9c9bc1784b97ac1c4cda6ae09";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array async base containers crackNum deepseq directory filepath mtl
    pretty process QuickCheck random syb template-haskell text time
    transformers uniplate
  ];
  testHaskellDepends = [
    base bytestring containers crackNum directory doctest filepath Glob
    hlint mtl QuickCheck random tasty tasty-golden tasty-hunit
    tasty-quickcheck
  ];
  testSystemDepends = [ z3 abc-verifier ];
  executableSystemDepends = [ z3 abc-verifier ];
  benchmarkHaskellDepends = [
    base bench-show containers crackNum deepseq directory filepath
    gauge mtl process random silently syb text time
  ];
  homepage = "http://leventerkok.github.com/sbv/";
  description = "SMT Based Verification: Symbolic Haskell theorem prover using SMT solving";
  license = stdenv.lib.licenses.bsd3;
}
