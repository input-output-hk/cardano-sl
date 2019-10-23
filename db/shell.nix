let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "cardano-db";
  packages = ps: [ ps.cardano-sl-db ];
})
