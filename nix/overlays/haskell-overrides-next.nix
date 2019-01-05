self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;

in
{

  ## Testing with upcoming GHC releases.

  haskellPackages844 =
    (withLocalPinPonHlint (self.haskell.packages.ghc844.extend (self: super:
      rec {
      }
  )));

  haskellPackages861 =
    (withLocalPinPon pinPonHlintPath (self.haskell.packages.ghc861.extend (self: super:
      rec {
      }
  )));

}
