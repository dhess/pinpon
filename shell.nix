{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-pretty, amazonka, amazonka-core
      , amazonka-ec2, amazonka-sns, base, bytestring, conduit
      , conduit-combinators, containers, doctest, exceptions, hlint
      , hspec, http-client, http-types, lens, lucid, mellon-core, mtl
      , network, optparse-applicative, QuickCheck, resourcet, servant
      , servant-client, servant-docs, servant-lucid, servant-server
      , servant-swagger, stdenv, swagger2, text, time, transformers
      , transformers-base, wai, warp
      }:
      mkDerivation {
        pname = "pinpon";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson aeson-pretty amazonka amazonka-core amazonka-sns base
          bytestring containers exceptions http-client http-types lens lucid
          mellon-core mtl QuickCheck resourcet servant servant-client
          servant-docs servant-lucid servant-server servant-swagger swagger2
          text time transformers transformers-base wai warp
        ];
        executableHaskellDepends = [
          amazonka amazonka-ec2 amazonka-sns base conduit conduit-combinators
          containers exceptions lens mtl network optparse-applicative text
          time transformers warp
        ];
        testHaskellDepends = [ base doctest hlint hspec ];
        homepage = "https://github.com/dhess/pinpon/";
        description = "A network-enabled doorbell service";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: with pkgs.haskell.lib; {
        swagger2 = dontHaddock super.swagger2;
      };
  };
  drv = modifiedHaskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
