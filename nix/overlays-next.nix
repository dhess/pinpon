let

  localLib = import ./lib.nix;

in
[
  localLib.fetchNixPkgsLibQuixoftic
  localLib.fetchHpio
  ./overlays/lib.nix
  ./overlays/haskell-overrides-next.nix
]
