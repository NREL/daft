{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "type-list";
  version = "0.5.0.0";
  sha256 = "ffba5f4b467e06e3c8dffc00614d78604e0bdcfe3921fb258cd5a8d6772c690c";
  libraryHaskellDepends = [ base ];
  preConfigure = ''
    sed -e '30d'                                                       -i type-list.cabal
    sed -e '29,75d;82,90d;97,105d;112,120d;126,134d;144,148d;154,259d' -i src/Data/Type/List.hs
  '';
  description = "Operations on type-level lists and tuples";
  license = stdenv.lib.licenses.bsd3;
}
