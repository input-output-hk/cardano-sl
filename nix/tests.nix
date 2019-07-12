{ commonLib, src, nixTools }:
let
  pkgs = commonLib.pkgs;
  cardanoWallet = nixTools.nix-tools.cexes.cardano-wallet.cardano-generate-swagger-file;
  stylish-haskell = nixTools.nix-tools.cexes.stylish-haskell.stylish-haskell;
  validateJson = pkgs.callPackage ../tools/src/validate-json {};
in {
  shellcheck = pkgs.callPackage commonLib.tests.shellcheck { inherit src; };
  hlint = pkgs.callPackage commonLib.tests.hlint {
    inherit src;
    projects = [
      "util"
      "binary"
      "crypto"
      "core"
      "db"
      "chain"
      "infra"
      "node"
      "tools"
      "client"
      "generator"
      "auxx"
      "explorer"
      "wallet"
      "cluster"
      "mnemonic"
      "x509"
    ];
  };
  stylishHaskell = pkgs.callPackage commonLib.tests.stylishHaskell {
    inherit src stylish-haskell;
  };
  yamlValidation = pkgs.callPackage ./tests/yamlValidation.nix { inherit (nixTools.nix-tools.cexes.yaml-validation) yamlValidation; };
  swaggerSchemaValidation = pkgs.callPackage ./tests/swaggerSchemaValidation.nix { inherit cardanoWallet validateJson; };
}
