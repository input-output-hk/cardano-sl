let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "cardano-lib";
  packages = ps: [ ps.cardano-sl ];
})
