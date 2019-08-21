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
              daft      = haskellPackagesNew.callPackage          ./default.nix { };
              raft      = haskellPackagesNew.callPackage ../../raft/default.nix { };
              vinyl     = haskellPackagesNew.callPackage            ./vinyl.nix { };
              type-list = haskellPackagesNew.callPackage        ./type-list.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
    daft = pkgs.haskell.packages.${compiler}.daft;
  }
