let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "cardano-infra";
  packages = ps: [ ps.cardano-sl-infra ];
})
