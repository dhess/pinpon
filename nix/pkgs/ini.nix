{ mkDerivation, attoparsec, base, fetchgit, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "ini";
  version = "0.3.5";
  src = fetchgit {
    url = "https://github.com/chrisdone/ini.git";
    sha256 = "06yb1vjpay228nbps37dcl4mjyg74c4x4h7bgvrnyq0adjib5win";
    rev = "477e0ffe76b43fab9dd9ff1ad4925b9047683eb0";
  };
  libraryHaskellDepends = [
    attoparsec base text unordered-containers
  ];
  homepage = "http://github.com/chrisdone/ini";
  description = "Quick and easy configuration files in the INI format";
  license = stdenv.lib.licenses.bsd3;
}
