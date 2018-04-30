self: super:

let

  inherit (self) lib;


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

  withLocalPinPon = localPinPonPath: hp: (hp.extend (self: super: (
    {
      pinpon = myCleanPackage (super.callPackage localPinPonPath {});
    }
  )));

  withGitHubAmazonka = hp: (hp.extend (self: super: ({
    amazonka = super.callPackage ../pkgs/amazonka.nix {};
    amazonka-core = super.callPackage ../pkgs/amazonka-core.nix {};
    amazonka-sns = super.callPackage ../pkgs/amazonka-sns.nix {};
    amazonka-test = super.callPackage ../pkgs/amazonka-test.nix {};
  })));

  withConduit12 = hp: (hp.extend (self: super: ({
    conduit = super.conduit_1_2_13_1;
    conduit-extra = super.conduit-extra_1_2_3_2;
    http-conduit = super.http-conduit_2_2_4;
    resourcet = super.resourcet_1_1_11;
    xml-conduit = super.xml-conduit_1_7_1_2;
  })));

in
{
  lib = (super.lib or {}) // {

    inherit withLocalPinPon withGitHubAmazonka withConduit12;

    maintainers = super.lib.maintainers // {
      dhess-qx = "Drew Hess <dhess-src@quixoftic.com>";
    };

  };
}
