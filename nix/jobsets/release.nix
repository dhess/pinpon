let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "aarch64-linux" ]
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
      meta.maintainer = pkgs.lib.maintainers.dhess-pers;
      constituents = with jobs; [
        haskellPackages.pinponHlint.x86_64-darwin
        haskellPackages.pinponHlint.x86_64-linux
        haskellPackages.pinponHlint.aarch64-linux
      ];
    };
  } // (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  inherit (jobs) nixpkgs;
}
