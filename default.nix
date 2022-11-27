let
  pkgs = import <nixpkgs> { };
  myGhc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    # relude
    # async
  ]);
in
pkgs.mkShell {
  buildInputs = with pkgs;
    [
      # (?) maybe something like this will be required isnted of just `ghc` one day
      # to install additional haskell stuff
      #
      # pkgs.haskellPackages.ghcWithPackages
      # (hpkgs: with hpkgs; [
      #   gi-gtk
      # ])

      ghc
      stack
      cabal-install
      zlib
      sqlite
      haskell-language-server
      nixpkgs-fmt
      haskellPackages.cabal-fmt
      haskellPackages.fourmolu
      haskellPackages.implicit-hie
    ];
  LANG = "C.UTF-8";
}
