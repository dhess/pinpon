self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon;
  inherit (haskell.lib) noHaddocks;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;

in
{

  ## Testing with upcoming GHC releases. Don't bother Haddock-ing
  ## these as they're unlikely to be cached by upstream Hydra. Also,
  ## jailbreak the whole thing as we're not particularly worried about
  ## that here; we just want things to build.

  haskellPackages841 =
    noHaddocks (withLocalPinPon pinPonHlintPath (self.haskell.packages.ghc841.extend (self: super:
      with haskell.lib;
      rec {
      }
    )));

}
