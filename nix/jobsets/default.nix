# Based on
# https://github.com/input-output-hk/iohk-ops/blob/df01a228e559e9a504e2d8c0d18766794d34edea/jobsets/default.nix

{ nixpkgs ? <nixpkgs>
, declInput ? {}
}:

let

  pinponUri = "https://github.com/quixoftic/pinpon.git";

  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };

  ## nixpkgs-stackage wants a <nixpkgs> path so that it can import
  ## Haskell functions that nixpkgs doesn't explicitly export.
  nixpkgs-src = builtins.fromJSON (builtins.readFile ../nixpkgs-src.json);
  nixpkgs-spec = {
    url = "https://github.com/${nixpkgs-src.owner}/${nixpkgs-src.repo}.git";
    rev = "${nixpkgs-src.rev}";
  };

  pkgs = import nixpkgs {};

  defaultSettings = {
    enabled = 1;
    hidden = false;
    keepnr = 10;
    schedulingshares = 100;
    checkinterval = 60;
    enableemail = false;
    emailoverride = "";
    nixexprpath = "nix/jobsets/release.nix";
    nixexprinput = "pinpon";
    description = "pinpon";
    inputs = {

      ## Note: the nixpkgs input here is for nixpkgs-stackage's
      ## <nixpkgs>. It is not used by pinpon.
      nixpkgs = mkFetchGithub "${nixpkgs-spec.url} ${nixpkgs-spec.rev}";

      pinpon = mkFetchGithub "${pinponUri} master";

    };
  };

  mkChannelAlt = pinponBranch: nixpkgsRev: nixpkgsStackageRev: {
    inputs = {

      ## Note: the nixpkgs input here is for nixpkgs-stackage's
      ## <nixpkgs>. It is not used by pinpon.
      nixpkgs = mkFetchGithub "${nixpkgs-spec.url} ${nixpkgs-spec.rev}";

      nixpkgs_override = mkFetchGithub "https://github.com/NixOS/nixpkgs-channels.git ${nixpkgsRev}";
      nixpkgs_stackage_override = mkFetchGithub "https://github.com/typeable/nixpkgs-stackage.git ${nixpkgsStackageRev}";
      pinpon = mkFetchGithub "${pinponUri} ${pinponBranch}";

    };
  };


  ## "next" builds; these are expected to fail from time to time and
  ## don't run as often. They also build from nixpkgs and not
  ## nixpkgs-channels as we always want to be testing the latest,
  ## greatest GHC pre-releases. (We don't bother overriding
  ## nixpkgs-stackage because it's not used in these evaluations.)

  mkNext = pinponBranch: nixpkgsRev: {
    nixexprpath = "nix/jobsets/next.nix";
    checkinterval = 60 * 60 * 24;
    inputs = {

      ## Note: does not depend on nixpkgs because we don't use
      ## nixpkgs-stackage for these builds.

      nixpkgs_override = mkFetchGithub "https://github.com/NixOS/nixpkgs.git ${nixpkgsRev}";
      pinpon = mkFetchGithub "${pinponUri} ${pinponBranch}";

    };
  };

  mainJobsets = with pkgs.lib; mapAttrs (name: settings: defaultSettings // settings) (rec {
    master = {};
    nixpkgs-unstable = mkChannelAlt "master" "nixpkgs-unstable" "master";

    # None currently.
    #next-ghc = mkNext "master" "master";
  });

  jobsetsAttrs = mainJobsets;

  jobsetJson = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);

in {
  jobsets = with pkgs.lib; pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toJSON declInput}
    EOF
    cp ${jobsetJson} $out
  '';
}
