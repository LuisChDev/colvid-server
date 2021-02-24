{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { }
, hoo ? false }:

## override permite alterar atributos de la derivación
# 'haskellPackages'
(pkgs.haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
    (self: super: {
      ghc = super.ghc
        // (if hoo then { withPackages = super.ghc.withHoogle; } else { });
      ghcWithPackages = self.ghc.withPackages;
    });
})).callPackage ./default.nix { }
