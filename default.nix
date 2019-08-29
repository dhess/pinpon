let

  lib = (import nix/lib);
  defaultPkgs = lib.nixpkgs {};

in

{ pkgs ? defaultPkgs }:

let

  pinponOverlays = self: super:
    lib.customisation.composeOverlays lib.overlays super;
  pinponOverlaysMaintainer = self: super:
    lib.customisation.composeOverlays lib.maintainerOverlays super;
  pinponNix = nix/pkgs/pinpon.nix;
  pinponNixMaintainer = nix/pkgs/pinpon-maintainer.nix;

  pinponPkgs = lib.customisation.composeOverlays (lib.singleton pinponOverlays) pkgs;
  pinponPkgsMaintainer = lib.customisation.composeOverlays (lib.singleton pinponOverlaysMaintainer) pkgs;

in
{
  # haskellPackages with the local pinpon package. Note that this
  # package set builds pinpon *without* maintainer tests.
  inherit (pinponPkgs) haskellPackages;

  # The path to the local pinpon.nix, in case you want to make your
  # own.
  inherit pinponNix pinponNixMaintainer;

  # Same as the above, except with the pinpon package in maintainer
  # mode.
  maintainer = {
    inherit (pinponPkgsMaintainer) haskellPackages;
    pinponNix = pinponNixMaintainer;
  };

  overlays.pinpon = pinponOverlays;
  overlays.pinponMaintainer = pinponOverlaysMaintainer;
}
