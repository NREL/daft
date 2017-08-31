{ mkDerivation, base, Cabal, containers, directory, filepath, mtl
, process, stdenv, syb, tasty, tasty-golden, template-haskell
, th-desugar
}:
mkDerivation {
  pname = "singletons";
  version = "2.1";
  sha256 = "0b213bn1zsjv57xz4460jxs0i85xd5i462v00iqzfb5n6sx99cmr";
  libraryHaskellDepends = [
    base containers mtl syb template-haskell th-desugar
  ];
  testHaskellDepends = [
    base Cabal directory filepath process tasty tasty-golden
  ];
  doCheck = false;
  homepage = "http://www.github.com/goldfirere/singletons";
  description = "A framework for generating singleton types";
  license = stdenv.lib.licenses.bsd3;
}
