{
  nixpkgs  ? null
, compiler ? "ghc822"
}:

(import ../shell.nix) {package = "daft-mongodb"; nixpkgs = nixpkgs; compiler = compiler;}
