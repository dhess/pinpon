## Here we build pinpon against Nixpkgs using one or more pre-release
## versions of GHC. The goal here is to get ahead of issues that might
## arise with new GHC releases.

let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "aarch64-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [ (import ../../next.nix) ];
  }
}:

with import (fixedNixPkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

  jobs = {

    ghc844 = pkgs.releaseTools.aggregate {
      name = "ghc844";
      meta.description = "pinpon built against nixpkgs haskellPackages using GHC 8.4.4";
      meta.maintainer = lib.maintainers.dhess-qx;
      constituents = with jobs; [
        haskellPackages844.pinpon.x86_64-darwin
        haskellPackages844.pinpon.x86_64-linux
        haskellPackages844.pinpon.aarch64-linux
      ];
    };

    ghc861 = pkgs.releaseTools.aggregate {
      name = "ghc861";
      meta.description = "pinpon built against nixpkgs haskellPackages using GHC 8.6.1";
      meta.maintainer = lib.maintainers.dhess-qx;
      constituents = with jobs; [
        haskellPackages861.pinpon.x86_64-darwin
        haskellPackages861.pinpon.x86_64-linux
        haskellPackages861.pinpon.aarch64-linux
      ];
    };

  } // (mapTestOn ({

    haskellPackages861 = packagePlatforms pkgs.haskellPackages861;
    haskellPackages844 = packagePlatforms pkgs.haskellPackages844;

  }));

in
{
  inherit (jobs) ghc861 ghc844;
}
// pkgs.lib.testing.enumerateConstituents jobs.ghc861
// pkgs.lib.testing.enumerateConstituents jobs.ghc844
