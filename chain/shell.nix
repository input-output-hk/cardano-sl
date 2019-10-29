let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "cardano-chain";
  packages = ps: [ ps.cardano-sl-chain ];
})
