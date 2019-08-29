let

  # From https://github.com/input-output-hk/iohk-ops/blob/e6f1ae95cdbfdd5c213aa0b9a1ef67150febc503/lib.nix
  
  fixedDhessLibNix =
  let
    try = builtins.tryEval <dhess_lib_nix>;
  in
    if try.success
      then builtins.trace "Using <dhess_lib_nix>" try.value
      else (import ./fetch-github.nix) { jsonSpec = builtins.readFile ./dhess-lib-nix-src.json; };

  dhess-lib-nix = (import fixedDhessLibNix) {};
  inherit (dhess-lib-nix) lib haskell;
  inherit (lib.fetchers) fixedNixpkgs;
  inherit (lib.dhess-lib-nix) nixpkgs;

  fixedHpio = lib.fetchers.fixedNixSrc "hpio_override" ./hpio-src.json;
  hpio = (import fixedHpio) {};

  ## Ignore local files that shouldn't contribute to the Nix hash.
  ## Ideally this would be based on the cabal sdist contents, but
  ## that's not easily do-able at the moment.

  filterSourceLocal = name: type: let baseName = baseNameOf (toString name); in ! (
    type == "directory" && (
      baseName == "scripts"
    ) ||
    type != "directory" && (
      baseName == "Makefile"
    )
  );
  cleanSourceLocal = src: lib.sources.cleanSourceWith { filter = filterSourceLocal; inherit src; };
  myCleanSource = src: cleanSourceLocal (lib.sources.cleanSourceAllExtraneous src);
  myCleanPackage = pkg: lib.sources.cleanPackage myCleanSource pkg;

  ## Haskell package combinators.

  withLocalPinPon = hp: (haskell.lib.properExtend hp (self: super: ({
    pinpon = myCleanPackage (super.callPackage ../pkgs/pinpon.nix {});
  })));
  withLocalPinPonMaintainer = hp: (haskell.lib.properExtend hp (self: super: ({
    pinpon = myCleanPackage (super.callPackage ../pkgs/pinpon-maintainer.nix {});
  })));

  overlays = [
    hpio.overlays.hpio
    (import ../overlays/haskell-overrides.nix)
  ];
  maintainerOverlays = [
    hpio.overlays.hpio
    (import ../overlays/haskell-overrides-maintainer.nix)
  ];

in lib //
{
  inherit fixedNixpkgs;
  inherit nixpkgs;
  inherit haskell;
  inherit withLocalPinPon withLocalPinPonMaintainer;
  inherit overlays maintainerOverlays;
}
