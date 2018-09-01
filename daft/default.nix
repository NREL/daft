{ mkDerivation, aeson, base, bytestring, containers, data-default
, deepseq, hashable, mtl, raft, stdenv, text, tostring, type-list
, unordered-containers, vinyl
}:
mkDerivation {
  pname = "daft";
  version = "0.4.14.6";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers data-default deepseq hashable mtl
    raft text tostring type-list unordered-containers vinyl
  ];
  license = stdenv.lib.licenses.unfree;
}
