{
  mkDerivation, stdenv, fetchurl,
  aeson, attoparsec, base, binary, bytestring, cereal, containers, data-default, ghc-prim, mtl, parallel, scientific, split, stm, text, time, tostring, zlib
}:
mkDerivation rec {
  pname = "raft";
  version = "0.4.0.0";
  src = fetchurl {
    name = "${pname}-${version}.tar.bz";
    url = "https://bitbucket.org/functionally/${pname}/downloads/${pname}-${version}.tar.gz";
    sha256 = "1945vrzgb6r3g5qb17rlfcqh68wp9dmmnaxbc2wsp5jyjcnr8mv5";
  };
  libraryHaskellDepends = [
    aeson attoparsec base binary bytestring cereal containers data-default ghc-prim mtl parallel scientific split stm text time tostring zlib
  ];
  isLibrary = true;
  isExecutable = false;
  homepage = "https://bitbucket.org/functionally/raft";
  description = "Miscellaneous Haskell utilities for data structures and data manipulation";
  license = stdenv.lib.licenses.mit;
}
