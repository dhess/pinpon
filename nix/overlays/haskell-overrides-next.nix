self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPonHlint;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks;

in
{

  ## Testing with upcoming GHC releases.

  haskellPackages844 =
    (withLocalPinPonHlint (self.haskell.packages.ghc844.extend (self: super:
      rec {
      }
  )));

  haskellPackages861 =
    (withLocalPinPonHlint (self.haskell.packages.ghc861.extend (self: super:
      rec {
      }
  )));

}
