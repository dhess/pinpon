{ mkDerivation, amazonka-core, base, bytestring, conduit
, conduit-extra, directory, exceptions, fetchgit, http-client
, http-conduit, http-types, ini, mmorph, monad-control, mtl
, resourcet, retry, stdenv, tasty, tasty-hunit, text, time
, transformers, transformers-base, transformers-compat, void
}:
mkDerivation {
  pname = "amazonka";
  version = "1.5.0";
  src = fetchgit {
    url = "https://github.com/quixoftic/amazonka";
    sha256 = "03bfpmpxabsqfbxxnhbpr27xp431rkffwih9yl56sz673m1ypkr1";
    rev = "fa05d67f7e91a6722af030cf705d65a93cccca31";
  };
  postUnpack = "sourceRoot+=/amazonka; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    amazonka-core base bytestring conduit conduit-extra directory
    exceptions http-client http-conduit http-types ini mmorph
    monad-control mtl resourcet retry text time transformers
    transformers-base transformers-compat void
  ];
  testHaskellDepends = [ base tasty tasty-hunit ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Comprehensive Amazon Web Services SDK";
  license = stdenv.lib.licenses.mpl20;
}
