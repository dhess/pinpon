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
    url = "https://github.com/naushadh/amazonka.git";
    sha256 = "0jx9v9wkidklrp2d4i496453c7c8i3zjfcsnbfg9x9dkw2fn8v8q";
    rev = "97273545cf37672bec0cdcf78a9d68c274bbb6c2";
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
