{ mkDerivation, base, bifunctors, bytestring, containers, deepseq
, filepath, gauge, hashable, megaparsec, monad-logger, mtl
, parser-combinators, QuickCheck, sbv, silently, stdenv, tasty
, tasty-golden, tasty-hspec, tasty-hunit, tasty-quickcheck, text
, transformers, unordered-containers, z3, zlib, unagi-chan
, async, directory, aeson
}:
mkDerivation {
  pname = "vsmt";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers deepseq hashable megaparsec monad-logger
    mtl parser-combinators sbv text transformers unordered-containers
    unagi-chan async
  ];
  librarySystemDepends    = [ z3 zlib ];
  executableSystemDepends = [ z3 zlib ];
  testHaskellDepends = [
    base bytestring containers filepath QuickCheck sbv tasty
    tasty-golden tasty-hspec tasty-hunit tasty-quickcheck text
    unordered-containers
  ];

  benchmarkHaskellDepends = [ base deepseq gauge silently megaparsec text
                              parser-combinators sbv containers directory
                              aeson
                            ];
  homepage = "https://github.com/doyougnu/VSmt";
  description = "SMT Based Verification with first class variational programming capabilities";
  license = stdenv.lib.licenses.bsd3;
}
