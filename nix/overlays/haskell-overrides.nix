self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon;
  inherit (haskell.lib) dontCheck;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;
  pinPonPath = ../pkgs/pinpon.nix;

in
{
  haskellPackages =
    withLocalPinPon pinPonHlintPath (super.haskellPackages.extend (self: super:
      rec {
        # Doesn't currently check.
        hpio = dontCheck super.hpio;
      }
  ));
}
