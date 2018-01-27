self: super:

let

  inherit (self) haskell;

in
{
  haskellPackages = super.haskellPackages.extend (self: super:
    with haskell.lib;
    rec {
      # Doesn't currently check.
      hpio = dontCheck super.hpio;

      pinpon = self.callPackage ../pkgs/pinpon.nix {};
    }
  );
}
