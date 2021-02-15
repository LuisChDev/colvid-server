{ pkgs ? import <nixpkgs> {},
  hoo ? false }:

(pkgs.haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
    ghc = super.ghc // (if hoo then { withPackages = super.ghc.withHoogle; } else {});
    ghcWithPackages = self.ghc.withPackages;
  });
})).callPackage ./default.nix {}