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
    url = "https://github.com/naushadh/amazonka.git";
    sha256 = "0jx9v9wkidklrp2d4i496453c7c8i3zjfcsnbfg9x9dkw2fn8v8q";
    rev = "97273545cf37672bec0cdcf78a9d68c274bbb6c2";
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
