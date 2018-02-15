let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "armv7l-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [ (import ../../.) ];
  }
}:

with import (fixedNixPkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

  jobs = {

    nixpkgs = pkgs.releaseTools.aggregate {
      name = "nixpkgs";
      meta.description = "pinpon built against nixpkgs haskellPackages";
      meta.maintainer = lib.maintainers.dhess-qx;
      constituents = with jobs; [
        haskellPackages.pinpon.x86_64-darwin
        haskellPackages.pinpon.x86_64-linux
        haskellPackagesArmv7l.pinpon.armv7l-linux
      ];
    };

    lts-10 = pkgs.releaseTools.aggregate {
      name = "lts-10";
      meta.description = "pinpon built against Stackage LTS 10 package set";
      meta.maintainers = pkgs.lib.maintainers.dhess-qx;
      constituents = with jobs; [
        lts10Packages.pinpon.x86_64-darwin
        lts10Packages.pinpon.x86_64-linux
      ];
    };

    lts-9 = pkgs.releaseTools.aggregate {
      name = "lts-9";
      meta.description = "mellon packages built against Stackage LTS 9 package set";
      meta.maintainers = pkgs.lib.maintainers.dhess-qx;
      constituents = with jobs; [
        lts9Packages.pinpon.x86_64-darwin
        lts9Packages.pinpon.x86_64-linux
      ];
    };

  } // (mapTestOn ({

    haskellPackages = packagePlatforms pkgs.haskellPackages;
    haskellPackagesArmv7l = packagePlatforms pkgs.haskellPackagesArmv7l;
    lts10Packages = packagePlatforms pkgs.lts10Packages;
    lts9Packages = packagePlatforms pkgs.lts9Packages;

  }));

in
{
  inherit (jobs) nixpkgs lts-10 lts-9;
}
