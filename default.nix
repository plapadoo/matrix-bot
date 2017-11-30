{ mkDerivation, aeson, attoparsec, base, bytestring, configurator
, containers, cryptonite, directory, filepath, free, http-types
, HUnit, iso8601-time, lens, matrix-bot-api, monad-loops, mtl
, optparse-applicative, plpd-utils, QuickCheck
, quickcheck-instances, random, scotty, stdenv, stm, test-framework
, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, text, time, transformers, uuid, wai, warp
, wreq
}:
mkDerivation {
  pname = "matrix-bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring configurator containers cryptonite
    directory filepath free http-types lens matrix-bot-api monad-loops
    mtl optparse-applicative plpd-utils scotty stm text transformers
    wai warp wreq
  ];
  executableHaskellDepends = [
    base iso8601-time lens mtl plpd-utils random text time uuid
  ];
  testHaskellDepends = [
    base filepath http-types HUnit lens matrix-bot-api monad-loops mtl
    plpd-utils QuickCheck quickcheck-instances test-framework
    test-framework-hunit test-framework-quickcheck2 test-framework-th
    text
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/pmiddend/matrix-bot#readme";
  license = stdenv.lib.licenses.bsd3;
}
