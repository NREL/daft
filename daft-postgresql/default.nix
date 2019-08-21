{
  mkDerivation, stdenv
, base, bytestring, contravariant, daft, data-default, hasql, semigroups, text, vinyl
}:
mkDerivation {
  pname = "daft-postgresql";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring contravariant daft data-default hasql semigroups text vinyl
  ];
  license = stdenv.lib.licenses.unfree;
}
