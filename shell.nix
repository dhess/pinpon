{ compiler ? "ghc822"
, overlays ? [ (import ./.) ]
}:

let

  fixedNixPkgs = (import ./nix/lib.nix).fetchNixPkgs;

  pkgs = (import fixedNixPkgs) { inherit overlays; };

  drv = pkgs.haskellPackages.pinpon;

in

  if pkgs.lib.inNixShell then drv.env else drv
