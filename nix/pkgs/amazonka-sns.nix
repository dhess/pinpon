{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, fetchgit, stdenv, tasty, tasty-hunit, text, time
, unordered-containers
}:
mkDerivation {
  pname = "amazonka-sns";
  version = "1.5.0";
  src = fetchgit {
    url = "https://github.com/quixoftic/amazonka";
    sha256 = "03bfpmpxabsqfbxxnhbpr27xp431rkffwih9yl56sz673m1ypkr1";
    rev = "fa05d67f7e91a6722af030cf705d65a93cccca31";
  };
  postUnpack = "sourceRoot+=/amazonka-sns; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ amazonka-core base ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring tasty tasty-hunit text
    time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Simple Notification Service SDK";
  license = stdenv.lib.licenses.mpl20;
}
