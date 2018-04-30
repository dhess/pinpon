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
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "11k9l7cmm6zv6nc3k0yf513zc4jgrx80av1szzrjdlcgiir6abag";
    rev = "9aa47c267631339a32a1a4ea373b1319145662e2";
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
