with import <nixpkgs> {};
(import ./release.nix { hoo = true; }).env.overrideAttrs (old: {
  buildInputs = [
    haskell-language-server
    cabal-install
    cabal2nix
  ];
})
