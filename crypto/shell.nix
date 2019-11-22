let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "cardano-sl-crypto";
  packages = ps: [ ps.cardano-sl-crypto ps.cabal-install ];
})
