let
  self = import ../. {};
in (self.nix-tools.shellFor {
  name = "cardano-wallet";
  packages = ps: [ ps.cardano-wallet ];
})
