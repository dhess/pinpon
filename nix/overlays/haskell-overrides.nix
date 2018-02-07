self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon;
  inherit (haskell.lib) dontCheck noHaddocks;

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
        # Doesn't currently check.
        hpio = dontCheck super.hpio;
      }
  ));

  # Currently, armv7l-linux on Nixpkgs must use ghc802.

  haskellPackagesArmv7l =
    withLocalPinPon pinPonPath (self.haskell.packages.ghc802.extend (self: super:
      {
        # Doesn't currently check.
        hpio = dontCheck super.hpio;
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
