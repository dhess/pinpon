{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, fetchgit, stdenv, tasty, tasty-hunit, text, time
, unordered-containers
}:
mkDerivation {
  pname = "amazonka-sns";
  version = "1.5.0";
  src = fetchgit {
    url = "https://github.com/naushadh/amazonka.git";
    sha256 = "0jx9v9wkidklrp2d4i496453c7c8i3zjfcsnbfg9x9dkw2fn8v8q";
    rev = "97273545cf37672bec0cdcf78a9d68c274bbb6c2";
  };
  postUnpack = "sourceRoot+=/amazonka-sns; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ amazonka-core base ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring tasty tasty-hunit text
    time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Simple Notification Service SDK";
  license = stdenv.lib.licenses.mpl20;
}
