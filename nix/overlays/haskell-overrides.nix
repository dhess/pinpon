self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon withGitHubAmazonka;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;
  pinPonPath = ../pkgs/pinpon.nix;

in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages =
    withGitHubAmazonka (withLocalPinPon pinPonHlintPath (super.haskellPackages.extend (self: super:
      rec {
      }
  )));


  ## Nixpkgs with GHC 8.4.1. Note that we use hlint tests here.

  haskellPackages841 =
    withGitHubAmazonka (withLocalPinPon pinPonHlintPath (self.haskell.packages.ghc841.extend (self: super:
      rec {
        http-media = doJailbreak super.http-media;
        servant = doJailbreak super.servant;
        servant-client = doJailbreak super.servant-client;
        servant-server = doJailbreak super.servant-server;
        servant-swagger-ui = doJailbreak super.servant-swagger-ui;
      }
    )));

}
