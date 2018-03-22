{ mkDerivation, aeson, aeson-qq, base, base-compat, bytestring
, Cabal, cabal-doctest, containers, doctest, generics-sop, Glob
, hashable, hspec, hspec-discover, http-media, HUnit
, insert-ordered-containers, lens, mtl, network, QuickCheck
, quickcheck-instances, scientific, stdenv, template-haskell, text
, time, transformers, transformers-compat, unordered-containers
, uuid-types, vector
}:
mkDerivation {
  pname = "swagger2";
  version = "2.2.1";
  sha256 = "1f97a5be1b8b5f23f22b7efc387f8a32e60bcfc787839bbebc882ff2c4f32086";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson base base-compat bytestring containers generics-sop hashable
    http-media insert-ordered-containers lens mtl network scientific
    template-haskell text time transformers transformers-compat
    unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    aeson aeson-qq base base-compat bytestring containers doctest Glob
    hashable hspec HUnit insert-ordered-containers lens mtl QuickCheck
    quickcheck-instances text time unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/GetShopTV/swagger2";
  description = "Swagger 2.0 data model";
  license = stdenv.lib.licenses.bsd3;
}
