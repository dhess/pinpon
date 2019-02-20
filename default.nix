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

in
{
  # haskellPackages with the local pinpon package. Note that this
  # package set builds pinpon *without* maintainer tests.
  inherit (pinponPkgs) haskellPackages;

  # The path to the local pinpon.nix (and pinpon-maintainer.nix, with
  # maintainer tests enabled), in case you want to make your own.
  inherit pinponNix pinponNixMaintainer;

  overlays.pinpon = pinponOverlays;
  overlays.pinponMaintainer = pinponOverlaysMaintainer;
}
