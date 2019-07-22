let
  commonLib = import ./lib.nix;
  disabled = [];
in { cardano ? { outPath = ./.; rev = "abcdef"; }, ... }@args:
let
  getArchDefault = system: let
    table = {
      x86_64-linux = import ./. { target = "x86_64-linux"; gitrev = cardano.rev; };
      x86_64-darwin = import ./. { target = "x86_64-darwin"; gitrev = cardano.rev; };
      x86_64-windows = import ./. { target = "x86_64-windows"; gitrev = cardano.rev; };
    };
  in table.${system};
  default = getArchDefault builtins.currentSystem;
  makeConnectScripts = cluster: let
    getScript = name: {
      x86_64-linux = (getArchDefault "x86_64-linux").connectScripts.${cluster}.${name};
      x86_64-darwin = (getArchDefault "x86_64-darwin").connectScripts.${cluster}.${name};
    };
  in {
    explorer = getScript "explorer";
    proposal-ui = getScript "proposal-ui";
    wallet = getScript "wallet";
  };
  wrapDockerImage = cluster: let
    images = (getArchDefault "x86_64-linux").dockerImages;
    wrapImage = image: commonLib.pkgs.runCommand "${image.name}-hydra" {} ''
      mkdir -pv $out/nix-support/
      cat <<EOF > $out/nix-support/hydra-build-products
      file dockerimage ${image}
      EOF
    '';
  in {
    wallet = wrapImage images.${cluster}.wallet;
    explorer = wrapImage images.${cluster}.explorer;
  };
  makeRelease = cluster: {
    name = cluster;
    value = {
      connectScripts = makeConnectScripts cluster;
      dockerImage = wrapDockerImage cluster;
    };
  };
in
commonLib.pkgs.lib.mapAttrsRecursiveCond
(as: !(as ? "type" && as.type == "derivation"))
(path: v: if (builtins.elem path disabled) then null else v)
(commonLib.nix-tools.release-nix {
  _this = cardano;
  package-set-path = ./nix/nix-tools.nix;
  packages = [
    "cardano-sl"
    "cardano-sl-auxx"
    "cardano-sl-chain"
    "cardano-sl-core"
    "cardano-sl-crypto"
    "cardano-sl-db"
    "cardano-sl-generator"
    "cardano-sl-infra"
    "cardano-sl-faucet"
    "cardano-sl-networking"
    "cardano-sl-node"
    "cardano-sl-tools"
    "cardano-sl-util"
    "cardano-sl-x509"
    "cardano-wallet"
    "cardano-sl-explorer"
    "cardano-sl-cluster"
  ];
  extraBuilds = {
    inherit (default) tests demoCluster explorerFrontend faucetFrontend explorerPythonAPI;
    daedalus-bridge = commonLib.pkgs.lib.mapAttrs (k: v: (getArchDefault k).daedalus-bridge) {
      x86_64-linux = 1;
      x86_64-darwin = 1;
      x86_64-windows = 1;
    };
  } // (builtins.listToAttrs (map makeRelease [ "mainnet" "staging" "demo" "testnet" ]));
  required-targets = jobs: [
    jobs.nix-tools.exes.cardano-sl-node.x86_64-linux
    jobs.nix-tools.exes.cardano-sl-auxx.x86_64-linux
    jobs.nix-tools.exes.cardano-sl-faucet.x86_64-linux
    jobs.nix-tools.exes.cardano-sl-explorer.x86_64-linux
    jobs.nix-tools.exes.cardano-wallet.x86_64-linux
    jobs.nix-tools.exes.cardano-wallet.x86_64-darwin
  ] ++ (builtins.attrValues jobs.tests)
  ++ (builtins.attrValues jobs.daedalus-bridge);
} (builtins.removeAttrs args ["cardano"]))
