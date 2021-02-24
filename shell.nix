with import (import ./nix/sources.nix).nixpkgs {};
(import ./release.nix { hoo = true; }).env.overrideAttrs (old: {
  buildInputs = [
    haskellPackages.haskell-language-server
    cabal-install
    cabal2nix
    niv
    nixops
    heroku
  ];
})
