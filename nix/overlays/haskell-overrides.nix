self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks properExtend;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;
  pinPonPath = ../pkgs/pinpon.nix;

in
{
  ## The default Nixpkgs package set. Note that we use hlint tests here.
  haskellPackages =
    (withLocalPinPon pinPonHlintPath (properExtend super.haskellPackages (self: super:
      {
        servant-docs = doJailbreak super.servant-docs;
        insert-ordered-containers = doJailbreak super.insert-ordered-containers;
        tdigest = doJailbreak super.tdigest;
      }
  )));
}
