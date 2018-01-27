{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-core
, amazonka-sns, base, bytestring, containers, doctest, exceptions
, hlint, hpio, hspec, http-client, http-client-tls, http-types
, lens, lucid, mtl, network, optparse-applicative, optparse-text
, QuickCheck, quickcheck-instances, resourcet, servant
, servant-client, servant-docs, servant-lucid, servant-server
, servant-swagger, servant-swagger-ui, stdenv, swagger2, text, time
, transformers, transformers-base, wai, warp
}:
mkDerivation {
  pname = "pinpon";
  version = "0.2.0.0";
  src = ../../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-core amazonka-sns base
    bytestring containers exceptions http-client http-types lens lucid
    mtl resourcet servant servant-client servant-docs servant-lucid
    servant-server servant-swagger servant-swagger-ui swagger2 text
    time transformers transformers-base wai warp
  ];
  executableHaskellDepends = [
    amazonka amazonka-sns base bytestring containers exceptions hpio
    http-client http-client-tls http-types lens mtl network
    optparse-applicative optparse-text servant-client text time
    transformers warp
  ];
  testHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-core amazonka-sns base
    bytestring containers doctest exceptions hlint hspec http-client
    http-types lens lucid mtl QuickCheck quickcheck-instances resourcet
    servant servant-client servant-docs servant-lucid servant-server
    servant-swagger servant-swagger-ui swagger2 text time transformers
    transformers-base wai warp
  ];
  homepage = "https://github.com/quixoftic/pinpon#readme";
  description = "A gateway for various cloud notification services";
  license = stdenv.lib.licenses.bsd3;
}
