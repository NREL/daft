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
              daft-postgresql = haskellPackagesNew.callPackage          ./default.nix { };
              daft            = haskellPackagesNew.callPackage    ../daft/default.nix { };
              raft            = haskellPackagesNew.callPackage ../../raft/default.nix { };
              vinyl           = haskellPackagesNew.callPackage      ../daft/vinyl.nix { };
              type-list       = haskellPackagesNew.callPackage  ../daft/type-list.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
    daft-postgresql = pkgs.haskell.packages.${compiler}.daft-postgresql;
  }
