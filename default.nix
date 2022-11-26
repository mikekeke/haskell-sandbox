let
  pkgs = import <nixpkgs> { };
  myGhc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    relude
    async
  ]);
in
pkgs.mkShell {
  buildInputs = with pkgs;
    [
      stack
      cabal-install
      myGhc
      zlib
      sqlite
      haskell-language-server
      nixpkgs-fmt
      haskellPackages.cabal-fmt
      haskellPackages.fourmolu
      haskellPackages.implicit-hie
    ];
   LANG="C.UTF-8";
}