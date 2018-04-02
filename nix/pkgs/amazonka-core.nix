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
    url = "https://github.com/naushadh/amazonka.git";
    sha256 = "0jx9v9wkidklrp2d4i496453c7c8i3zjfcsnbfg9x9dkw2fn8v8q";
    rev = "97273545cf37672bec0cdcf78a9d68c274bbb6c2";
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
