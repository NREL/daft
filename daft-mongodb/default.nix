{
  mkDerivation, stdenv
, base, bson, bson-generic, bytestring, daft, monad-control, mongoDB, mtl, text, vinyl
}:
mkDerivation {
  pname = "daft-mongodb";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bson bson-generic bytestring daft monad-control mongoDB mtl text vinyl
  ];
  license = stdenv.lib.licenses.unfree;
}
