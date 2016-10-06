{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, amazonka, amazonka-ec2, amazonka-sns
      , base, bytestring, conduit, conduit-combinators, doctest
      , exceptions, hlint, hspec, http-client, http-types, lens, lucid
      , mellon-core, mtl, network, optparse-applicative, servant
      , servant-client, servant-docs, servant-lucid, servant-server
      , stdenv, text, time, transformers, wai, warp
      }:
      mkDerivation {
        pname = "pinpon";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson amazonka amazonka-sns base bytestring http-client http-types
          lens lucid mellon-core servant servant-client servant-docs
          servant-lucid servant-server text time transformers wai warp
        ];
        executableHaskellDepends = [
          amazonka amazonka-ec2 amazonka-sns base conduit conduit-combinators
          exceptions lens mtl network optparse-applicative text time
          transformers warp
        ];
        testHaskellDepends = [ base doctest hlint hspec ];
        homepage = "https://github.com/dhess/pinpon/";
        description = "A network-enabled doorbell service";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
