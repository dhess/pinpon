self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;
  pinPonPath = ../pkgs/pinpon.nix;

  ## pinpon adds a few extra-deps to the Stackage LTS sets.

  withLts9Extras = hp: (hp.extend (self: super: (
    rec {
      protolude = self.callPackage ../pkgs/protolude-0.2.nix {};
    }
  )));

in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages =
    withLocalPinPon pinPonHlintPath (super.haskellPackages.extend (self: super:
      rec {
      }
  ));


  ## Nixpkgs with GHC 8.4.1. Note that we use hlint tests here.

  haskellPackages841 =
    withLocalPinPon pinPonHlintPath (self.haskell.packages.ghc841.extend (self: super:
      with haskell.lib;
      rec {
        http-media = doJailbreak super.http-media;
        servant = doJailbreak super.servant;
        servant-client = doJailbreak super.servant-client;
        servant-server = doJailbreak super.servant-server;
        servant-swagger = doJailbreak super.servant-swagger;
        servant-swagger-ui = doJailbreak super.servant-swagger-ui;
        swagger2 = super.callPackage ../pkgs/swagger2-2.2.1.nix {};
      }
    ));


  # Currently, armv7l-linux on Nixpkgs must use ghc802.

  haskellPackagesArmv7l =
    withLocalPinPon pinPonPath (self.haskell.packages.ghc802.extend (self: super:
      {
        # Fix issues on more recent nixpkgs.
        concurrent-output = doJailbreak super.concurrent-output;
        hedgehog = dontCheck super.hedgehog;
      }
    ));


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.

  lts10Packages =
    withLocalPinPon pinPonPath (self.haskell.packages.stackage.lts-104.extend (self: super:
      {
      }
    ));

  # Don't waste time Haddock-ing these.

  lts9Packages =
    noHaddocks (withLocalPinPon pinPonPath (self.haskell.packages.stackage.lts-921.extend (self: super:
      {
        protolude = self.callPackage ../pkgs/protolude-0.2.nix {};

        # Doesn't check.
        zlib = dontCheck super.zlib;
      }
    )));

}
