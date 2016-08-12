with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "hashed";
  version = "1";
  buildInputs = [
    (haskellPackages.ghcWithPackages
      (pkgs: with pkgs; [
        free
        pureMD5
      ]))
  ];
}
