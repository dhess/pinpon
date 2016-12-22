{ mkDerivation, acid-state, aeson, aeson-pretty, amazonka
, amazonka-core, amazonka-ec2, amazonka-sns, base, bytestring
, conduit, conduit-combinators, containers, doctest, exceptions
, hlint, hspec, http-api-data, http-client, http-types, lens, lucid
, mellon-core, mtl, network, optparse-applicative, QuickCheck
, quickcheck-instances, resourcet, safecopy, servant
, servant-client, servant-docs, servant-lucid, servant-server
, servant-swagger, servant-swagger-ui, stdenv, stm, swagger2, text
, time, transformers, transformers-base, wai, warp
}:
mkDerivation {
  pname = "pinpon";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state aeson aeson-pretty amazonka amazonka-core amazonka-sns
    base bytestring containers exceptions http-api-data http-client
    http-types lens lucid mellon-core mtl resourcet safecopy servant
    servant-client servant-docs servant-lucid servant-server
    servant-swagger servant-swagger-ui stm swagger2 text time
    transformers transformers-base wai warp
  ];
  executableHaskellDepends = [
    acid-state amazonka amazonka-ec2 amazonka-sns base conduit
    conduit-combinators containers exceptions lens mtl network
    optparse-applicative stm text time transformers warp
  ];
  testHaskellDepends = [
    acid-state aeson aeson-pretty amazonka amazonka-core amazonka-sns
    base bytestring containers doctest exceptions hlint hspec
    http-api-data http-client http-types lens lucid mellon-core mtl
    QuickCheck quickcheck-instances resourcet safecopy servant
    servant-client servant-docs servant-lucid servant-server
    servant-swagger servant-swagger-ui stm swagger2 text time
    transformers transformers-base wai warp
  ];
  homepage = "https://github.com/dhess/pinpon/";
  description = "A gateway for various cloud notification services";
  license = stdenv.lib.licenses.bsd3;
}
