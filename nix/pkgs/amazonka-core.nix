{ mkDerivation, aeson, attoparsec, base, bifunctors, bytestring
, case-insensitive, conduit, conduit-extra, cryptonite
, data-ordlist, deepseq, exceptions, fetchgit, hashable
, http-client, http-conduit, http-types, lens, memory, mtl
, QuickCheck, quickcheck-unicode, resourcet, scientific, semigroups
, stdenv, tagged, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text, time, transformers, transformers-compat
, unordered-containers, xml-conduit, xml-types
}:
mkDerivation {
  pname = "amazonka-core";
  version = "1.5.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "11k9l7cmm6zv6nc3k0yf513zc4jgrx80av1szzrjdlcgiir6abag";
    rev = "9aa47c267631339a32a1a4ea373b1319145662e2";
  };
  postUnpack = "sourceRoot+=/core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base bifunctors bytestring case-insensitive
    conduit conduit-extra cryptonite deepseq exceptions hashable
    http-client http-conduit http-types lens memory mtl resourcet
    scientific semigroups tagged text time transformers
    transformers-compat unordered-containers xml-conduit xml-types
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive conduit data-ordlist
    http-conduit http-types lens QuickCheck quickcheck-unicode tasty
    tasty-hunit tasty-quickcheck template-haskell text time
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Core data types and functionality for Amazonka libraries";
  license = stdenv.lib.licenses.mpl20;
}
