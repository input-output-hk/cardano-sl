let
  cardanoPkgs = import ../default.nix {};
  cfgFiles = cardanoPkgs.pkgs.runCommand "cfg" {} ''
    mkdir $out
    cd $out
    cp ${../lib/configuration.yaml} configuration.yaml
    cp ${../lib/mainnet-genesis.json} mainnet-genesis.json
    cp ${../lib/testnet-genesis.json} testnet-genesis.json
    cp ${../lib/mainnet-genesis-dryrun-with-stakeholders.json} mainnet-genesis-dryrun-with-stakeholders.json
  '';
  makeHelper = cfg: cardanoPkgs.pkgs.writeScriptBin "test-gui-${cfg.name}" ''
    #!/bin/sh

    BIN=$(realpath dist/build/testcases/testcases)

    mkdir -pv states/script-runner/stack-gui-${cfg.name}
    cd states/script-runner/stack-gui-${cfg.name}

    SCRIPT=none $BIN --configuration-file ${cfgFiles}/configuration.yaml --configuration-key ${cfg.key} --log-console-off --db-path db --keyfile secret.key --log-config ${./log-config.yaml} --logs-prefix logs --system-start 1544614190 --topology ${./. + "/topology-${cfg.name}.yaml"}
  '';
  mainnet = makeHelper { name = "mainnet"; key = "mainnet_full"; };
  testnet = makeHelper { name = "testnet"; key = "testnet_full"; };
  staging = makeHelper { name = "staging"; key = "mainnet_dryrun_full"; };
  obft-testnet = makeHelper { name = "obft-testnet"; key = "obft_testnet"; };
in
  cardanoPkgs.cardano-sl-script-runner.env.overrideAttrs (drv: {
    buildInputs = drv.buildInputs ++ [ cardanoPkgs.cardano-sl-node-static cardanoPkgs.cardano-sl-tools mainnet testnet staging obft-testnet ];
  })
