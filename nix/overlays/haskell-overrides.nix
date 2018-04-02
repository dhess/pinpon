self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;
  pinPonPath = ../pkgs/pinpon.nix;

  withUnofficialAmazonka = hp: (hp.extend (self: super: ({
    amazonka = super.callPackage ../pkgs/amazonka.nix {};
    amazonka-core = super.callPackage ../pkgs/amazonka-core.nix {};
    amazonka-sns = super.callPackage ../pkgs/amazonka-sns.nix {};
    amazonka-test = super.callPackage ../pkgs/amazonka-test.nix {};
  })));

  withConduit12 = hp: (hp.extend (self: super: ({
    conduit = super.conduit_1_2_13_1;
    conduit-extra = super.conduit-extra_1_2_3_2;
    http-conduit = super.http-conduit_2_2_4;
    resourcet = super.resourcet_1_1_11;
    xml-conduit = super.xml-conduit_1_7_1_2;
  })));

in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages =
    withConduit12 (withLocalPinPon pinPonHlintPath (super.haskellPackages.extend (self: super:
      rec {
      }
  )));


  ## Nixpkgs with GHC 8.4.1. Note that we use hlint tests here.

  haskellPackages841 =
    withConduit12 (withLocalPinPon pinPonHlintPath (self.haskell.packages.ghc841.extend (self: super:
      rec {
        http-media = doJailbreak super.http-media;
        servant = doJailbreak super.servant;
        servant-client = doJailbreak super.servant-client;
        servant-server = doJailbreak super.servant-server;
        servant-swagger-ui = doJailbreak super.servant-swagger-ui;

        servant-swagger = super.callPackage ../pkgs/servant-swagger.nix {};
        swagger2 = super.callPackage ../pkgs/swagger2-2.2.1.nix {};
        ini = super.callPackage ../pkgs/ini.nix {};
      }
    )));


  # Currently, armv7l-linux on Nixpkgs must use ghc802.

  haskellPackagesArmv7l =
    withConduit12 (withLocalPinPon pinPonPath (self.haskell.packages.ghc802.extend (self: super:
      {
        concurrent-output = doJailbreak super.concurrent-output;
        hedgehog = dontCheck super.hedgehog;
        monad-logger = doJailbreak super.monad-logger;
      }
    )));

}
