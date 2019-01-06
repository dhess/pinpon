self: super:

let

  inherit (self) lib;
  inherit (self.haskell.lib) properExtend;


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

  withLocalPinPon = localPinPonPath: hp: (properExtend hp (self: super: (
    {
      pinpon = myCleanPackage (super.callPackage localPinPonPath {});
    }
  )));

in
{
  lib = (super.lib or {}) // {

    inherit withLocalPinPon;

    maintainers = super.lib.maintainers // {
      dhess-qx = "Drew Hess <src@drewhess.com>";
    };

  };
}
