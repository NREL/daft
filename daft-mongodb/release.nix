{
  nixpkgs  ? import <nixpkgs>
, compiler ? "ghc822"
}:

let
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              daft-mongodb = haskellPackagesNew.callPackage          ./default.nix { };
              daft         = haskellPackagesNew.callPackage    ../daft/default.nix { };
              raft         = haskellPackagesNew.callPackage ../../raft/default.nix { };
              vinyl        = haskellPackagesNew.callPackage      ../daft/vinyl.nix { };
              type-list    = haskellPackagesNew.callPackage  ../daft/type-list.nix { };
              bson-generic = haskellPackagesNew.callPackage     ./bson-generic.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
    daft-mongodb = pkgs.haskell.packages.${compiler}.daft-mongodb;
  }
