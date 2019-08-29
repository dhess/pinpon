## Build pinpon in maintainer mode.

self: super:

let

  lib = (import ../lib);
  inherit (lib) haskell withLocalPinPonMaintainer;
  inherit (haskell.lib) doJailbreak properExtend;

in
{
  ## The default Nixpkgs package set.
  haskellPackages =
    (withLocalPinPonMaintainer (properExtend super.haskellPackages (self: super:
      {
        amazonka-core = doJailbreak super.amazonka-core;
        amazonka = doJailbreak super.amazonka;
        servant-docs = doJailbreak super.servant-docs;
        insert-ordered-containers = doJailbreak super.insert-ordered-containers;
        tdigest = doJailbreak super.tdigest;
      }
  )));
}
