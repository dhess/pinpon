self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon withLocalPinPonHlint;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks;

  ## Useful if any overrides are needed for Stackage LTS sets.

  withLts12Extras = hp: (hp.extend (self: super: (
    rec {
    }
  )));

in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages =
    (withLocalPinPonHlint (super.haskellPackages.extend (self: super:
      rec {
      }
  )));


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.

  lts12Packages = (withLocalPinPonHlint (withLts12Extras self.haskell.packages.stackage.lts-124));

}
