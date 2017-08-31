{ compiler ? "ghc7103" }:

let
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              daft =
                haskellPackagesNew.callPackage ./default.nix { };
              singletons =
                haskellPackagesNew.callPackage ./singletons.nix { };
              raft =
                haskellPackagesNew.callPackage ../raft/default.nix { };
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
