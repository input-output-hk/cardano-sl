let
  cardanoPkgs = import ../. {};
  cfgFiles = cardanoPkgs.pkgs.runCommand "cfg" {} ''
    mkdir $out
    cd $out
    cp ${../lib/configuration.yaml} configuration.yaml
    cp ${../lib/mainnet-genesis.json} mainnet-genesis.json
    cp ${../lib/testnet-genesis.json} testnet-genesis.json
    cp ${../lib/mainnet-genesis-dryrun-with-stakeholders.json} mainnet-genesis-dryrun-with-stakeholders.json
  '';
  makeHelper = cfg: cardanoPkgs.pkgs.writeScriptBin "test-wallet-${cfg.name}" ''
    #!/bin/sh

    set -e

    runhaskell Setup.hs build cardano-node

    mkdir -pv states/${cfg.name}/tls/{server,client}
    STATE=states/${cfg.name}

    ${cardanoPkgs.cardano-sl-tools}/bin/cardano-x509-certificates --server-out-dir $STATE/tls/server --clients-out-dir $STATE/tls/client --configuration-file ${cfgFiles}/configuration.yaml --configuration-key ${cfg.key}

    dist/build/cardano-node/cardano-node --configuration-file ${cfgFiles}/configuration.yaml --configuration-key ${cfg.key} \
        --db-path states/${cfg.name}/DB --keyfile states/${cfg.name}/secret.key \
        --wallet-db-path states/${cfg.name}/Wallet \
        --logs-prefix states/${cfg.name}/logs --topology ${../script-runner/. + "/topology-${cfg.name}.yaml"} \
        --tlscert $STATE/tls/server/server.crt --tlskey $STATE/tls/server/server.key \
        --tlsca $STATE/tls/server/ca.crt
  '';
  mainnet = makeHelper { name = "mainnet"; key = "mainnet_full"; };
  staging = makeHelper { name = "staging"; key = "mainnet_dryrun_full"; };
  testnet = makeHelper { name = "testnet"; key = "testnet_full"; };
in cardanoPkgs.cardano-wallet.env.overrideAttrs (drv: {
  buildInputs = drv.buildInputs ++ [ mainnet staging testnet ];
})
