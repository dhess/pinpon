{ mkDerivation, aeson, amazonka-core, base, bifunctors, bytestring
, case-insensitive, conduit, conduit-extra, fetchgit, groom
, http-client, http-types, process, resourcet, stdenv, tasty
, tasty-hunit, template-haskell, temporary, text, time
, unordered-containers, yaml
}:
mkDerivation {
  pname = "amazonka-test";
  version = "1.5.0";
  src = fetchgit {
    url = "https://github.com/quixoftic/amazonka";
    sha256 = "03bfpmpxabsqfbxxnhbpr27xp431rkffwih9yl56sz673m1ypkr1";
    rev = "fa05d67f7e91a6722af030cf705d65a93cccca31";
  };
  postUnpack = "sourceRoot+=/test; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson amazonka-core base bifunctors bytestring case-insensitive
    conduit conduit-extra groom http-client http-types process
    resourcet tasty tasty-hunit template-haskell temporary text time
    unordered-containers yaml
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Common functionality for Amazonka library test-suites";
  license = stdenv.lib.licenses.mpl20;
}
