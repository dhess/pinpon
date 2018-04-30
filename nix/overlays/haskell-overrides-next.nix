self: super:

let

  inherit (self) haskell;
  inherit (self.lib) withLocalPinPon withGitHubAmazonka;
  inherit (haskell.lib) dontCheck doJailbreak noHaddocks;

  pinPonHlintPath = ../pkgs/pinpon-hlint.nix;

in
{

  ## Testing with upcoming GHC releases.

  ## GHC 8.4.2.

  haskellPackages842 =
    withGitHubAmazonka (withLocalPinPon pinPonHlintPath (self.haskell.packages.ghc842.extend (self: super:
      rec {
        http-media = doJailbreak super.http-media;
        servant = doJailbreak super.servant;
        servant-client = doJailbreak super.servant-client;
        servant-server = doJailbreak super.servant-server;
        servant-swagger-ui = doJailbreak super.servant-swagger-ui;
      }
    )));

}
