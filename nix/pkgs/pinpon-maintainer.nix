{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-core
, amazonka-sns, base, bytestring, containers, doctest, exceptions
, hlint, hpack, hpio, hspec, http-client, http-client-tls
, http-types, lens, lucid, mtl, network, optparse-applicative
, optparse-text, protolude, QuickCheck, quickcheck-instances
, resourcet, servant, servant-client, servant-client-core
, servant-docs, servant-lucid, servant-server, servant-swagger
, servant-swagger-ui, stdenv, swagger2, text, time, transformers
, transformers-base, wai, warp
}:
mkDerivation {
  pname = "pinpon";
  version = "0.2.0.2";
  src = ../../.;
  configureFlags = [ "-ftest-hlint" ];
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-core amazonka-sns base
    bytestring containers exceptions http-client http-types lens lucid
    mtl protolude resourcet servant servant-client servant-client-core
    servant-docs servant-lucid servant-server servant-swagger
    servant-swagger-ui swagger2 text time transformers
    transformers-base wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    amazonka amazonka-sns base bytestring containers exceptions hpio
    http-client http-client-tls http-types lens mtl network
    optparse-applicative optparse-text protolude servant-client
    servant-client-core text time transformers warp
  ];
  testHaskellDepends = [
    aeson base bytestring doctest exceptions hlint hspec protolude
    QuickCheck quickcheck-instances servant-swagger
  ];
  prePatch = "hpack";
  homepage = "https://github.com/dhess/pinpon#readme";
  description = "A gateway for various cloud notification services";
  license = stdenv.lib.licenses.bsd3;
}
