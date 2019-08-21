{
  mkDerivation, stdenv
, aeson, base, bytestring, containers, data-default, deepseq, hashable, mtl, raft, text, tostring, type-list, unordered-containers, vinyl
}:
mkDerivation {
  pname = "daft";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers data-default deepseq hashable mtl raft text tostring type-list unordered-containers vinyl
  ];
  license = stdenv.lib.licenses.unfree;
}
