{ mkDerivation, amazonka, amazonka-ec2, amazonka-sns, base, conduit
, conduit-combinators, doctest, exceptions, hlint, hspec, lens
, optparse-applicative, stdenv, text, time
}:
mkDerivation {
  pname = "pinpon";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ amazonka amazonka-sns base lens text ];
  executableHaskellDepends = [
    amazonka amazonka-ec2 amazonka-sns base conduit conduit-combinators
    exceptions lens optparse-applicative text time
  ];
  testHaskellDepends = [ base doctest hlint hspec ];
  homepage = "https://github.com/dhess/pinpon/";
  description = "A network-enabled doorbell service";
  license = stdenv.lib.licenses.bsd3;
}
