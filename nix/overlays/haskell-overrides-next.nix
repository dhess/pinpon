self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon;
  inherit (haskell.lib) noHaddocks;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;

in
{

  ## Testing with upcoming GHC releases.

  # None currently.

}
