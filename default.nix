{ mkDerivation, base, doctest, hlint, hspec, optparse-applicative
, stdenv
}:
mkDerivation {
  pname = "pinpon";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [ base doctest hlint hspec ];
  license = stdenv.lib.licenses.bsd3;
}
