# Based on
# https://github.com/input-output-hk/iohk-ops/blob/df01a228e559e9a504e2d8c0d18766794d34edea/jobsets/default.nix

{ nixpkgs ? <nixpkgs>
, declInput ? {}
}:

let

  pinponUri = "https://github.com/dhess/pinpon.git";

  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };

  pkgs = import nixpkgs {};

  defaultSettings = {
    enabled = 1;
    hidden = false;
    keepnr = 10;
    schedulingshares = 100;
    checkinterval = 300;
    enableemail = false;
    emailoverride = "";
    nixexprpath = "nix/jobsets/release.nix";
    nixexprinput = "pinpon";
    description = "pinpon";
    inputs = {
      pinpon = mkFetchGithub "${pinponUri} master";
    };
  };

  mkChannelAlt = pinponBranch: nixpkgsRev: {
    inputs = {
      nixpkgs_override = mkFetchGithub "https://github.com/NixOS/nixpkgs-channels.git ${nixpkgsRev}";
      pinpon = mkFetchGithub "${pinponUri} ${pinponBranch}";

    };
  };


  ## "next" builds; these are expected to fail from time to time and
  ## don't run as often. They also build from nixpkgs and not
  ## nixpkgs-channels as we always want to be testing the latest,
  ## greatest GHC pre-releases.

  mkNext = pinponBranch: nixpkgsRev: {
    nixexprpath = "nix/jobsets/next.nix";
    checkinterval = 60 * 60 * 24;
    inputs = {
      pinpon = mkFetchGithub "${pinponUri} ${pinponBranch}";
    };
  };

  mainJobsets = with pkgs.lib; mapAttrs (name: settings: defaultSettings // settings) (rec {
    master = {};
    nixpkgs-unstable = mkChannelAlt "master" "nixpkgs-unstable";
    next-ghc = mkNext "master" "master";
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
