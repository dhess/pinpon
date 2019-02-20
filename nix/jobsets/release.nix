## The full set of packages we build/test, both on Hydra and for more
## extensive interactive development and testing. This file will
## create Hydra-style jobs for pinpon built against a fixed Nixpkgs
## Haskell package set.

let

  lib = import ../lib;
  fixedNixpkgs = lib.fixedNixpkgs;
  localPkgs = (import ../..) {};

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "aarch64-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [ localPkgs.overlays.pinponMaintainer ];
  }
}:

with import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

  all = pkg: lib.testing.enumerateSystems pkg supportedSystems;

  jobs = {
    nixpkgs = pkgs.releaseTools.aggregate {
      name = "nixpkgs";
      meta.description = "pinpon built against nixpkgs haskellPackages";
      meta.maintainer = lib.maintainers.dhess-pers;
      constituents = with jobs; [
        (all haskellPackages.pinpon)
      ];
    };
  } // (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  inherit (jobs) nixpkgs;
  inherit (jobs.haskellPackages) pinpon;
}
