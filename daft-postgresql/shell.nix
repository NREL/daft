{
  nixpkgs  ? null
, compiler ? "ghc822"
}:

(import ../shell.nix) {package = "daft-postgresql"; nixpkgs = nixpkgs; compiler = compiler;}
