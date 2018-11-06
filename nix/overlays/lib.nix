self: super:

let

  inherit (self) haskell lib;


  ## Ignore local files that shouldn't contribute to the Nix hash.
  ## Ideally this would be based on the cabal sdist contents, but
  ## that's not easily do-able at the moment.

  filterSourceLocal = name: type: let baseName = baseNameOf (toString name); in ! (
    type != "directory" && (
      baseName == "Makefile"
    )
  );
  cleanSourceLocal = src: lib.cleanSourceWith { filter = filterSourceLocal; inherit src; };

  myCleanSource = src: cleanSourceLocal (lib.cleanSourceAllExtraneous src);
  myCleanPackage = pkg: lib.cleanPackage myCleanSource pkg;


  ## Haskell package combinators.

  # On aarch64, pinpon tests trigger
  # https://ghc.haskell.org/trac/ghc/ticket/15275

  dontCheckAarch64 = pkg: if super.stdenv.hostPlatform.isAarch64 then (haskell.lib.dontCheck pkg) else pkg;

  withLocalPinPon = hp: (hp.extend (self: super: (
    {
      pinpon = myCleanPackage (dontCheckAarch64 (super.callCabal2nix "pinpon" ../../. {}));
    }
  )));

  withLocalPinPonHlint = hp: (hp.extend (self: super: (
    {
    pinpon = myCleanPackage (dontCheckAarch64 (super.callCabal2nixWithOptions "pinpon" ../../. "--flag test-hlint" {}));
    }
  )));

in
{
  lib = (super.lib or {}) // {

    inherit withLocalPinPon withLocalPinPonHlint;

    maintainers = super.lib.maintainers // {
      dhess-qx = "Drew Hess <src@drewhess.com>";
    };

  };
}
