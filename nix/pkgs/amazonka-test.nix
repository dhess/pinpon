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
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "11k9l7cmm6zv6nc3k0yf513zc4jgrx80av1szzrjdlcgiir6abag";
    rev = "9aa47c267631339a32a1a4ea373b1319145662e2";
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
