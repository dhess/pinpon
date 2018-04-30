{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, fetchgit, stdenv, tasty, tasty-hunit, text, time
, unordered-containers
}:
mkDerivation {
  pname = "amazonka-sns";
  version = "1.5.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "11k9l7cmm6zv6nc3k0yf513zc4jgrx80av1szzrjdlcgiir6abag";
    rev = "9aa47c267631339a32a1a4ea373b1319145662e2";
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
