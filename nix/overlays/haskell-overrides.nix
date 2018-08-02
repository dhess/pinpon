self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;
  pinPonPath = ../pkgs/pinpon.nix;

in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages =
    (withLocalPinPon pinPonHlintPath (super.haskellPackages.extend (self: super:
      rec {
      }
  )));

}
