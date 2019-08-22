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
              daft            = haskellPackagesNew.callPackage            ./daft/default.nix { };
              daft-mongodb    = haskellPackagesNew.callPackage    ./daft-mongodb/default.nix { };
              daft-postgresql = haskellPackagesNew.callPackage ./daft-postgresql/default.nix { };
              bson-generic    = haskellPackagesNew.callPackage            ./bson-generic.nix { };
              raft            = haskellPackagesNew.callPackage                    ./raft.nix { };
              type-list       = haskellPackagesNew.callPackage               ./type-list.nix { };
              vinyl           = haskellPackagesNew.callPackage                   ./vinyl.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
    daft            = pkgs.haskell.packages.${compiler}.daft           ;
    daft-mongodb    = pkgs.haskell.packages.${compiler}.daft-mongodb   ;
    daft-postgresql = pkgs.haskell.packages.${compiler}.daft-postgresql;
  }
