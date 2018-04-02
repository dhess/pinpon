{ mkDerivation, aeson, aeson-qq, base, bytestring, Cabal
, cabal-doctest, directory, doctest, fetchgit, filepath, hspec
, hspec-discover, http-media, insert-ordered-containers, lens
, QuickCheck, servant, singleton-bool, stdenv, swagger2, text, time
, unordered-containers
}:
mkDerivation {
  pname = "servant-swagger";
  version = "1.1.5";
  src = fetchgit {
    url = "https://github.com/haskell-servant/servant-swagger.git";
    sha256 = "1zd79sz5n9hv6b034kp66xh1q1prh745dxic356f34gny48bjjpn";
    rev = "3d5faab71d1a0682c93174388075d729bd0de726";
  };
  revision = "1";
  editedCabalFile = "005b3z7wxcrad8210yw3qsndh3zh0v2h8j50qxl8sj1l6wqb7zs6";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson base bytestring hspec http-media insert-ordered-containers
    lens QuickCheck servant singleton-bool swagger2 text
    unordered-containers
  ];
  testHaskellDepends = [
    aeson aeson-qq base directory doctest filepath hspec lens
    QuickCheck servant swagger2 text time
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell-servant/servant-swagger";
  description = "Generate Swagger specification for your servant API";
  license = stdenv.lib.licenses.bsd3;
}
