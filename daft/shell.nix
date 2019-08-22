{
  nixpkgs  ? null
, compiler ? "ghc822"
}:

(import ../shell.nix) {package = "daft"; nixpkgs = nixpkgs; compiler = compiler;}
