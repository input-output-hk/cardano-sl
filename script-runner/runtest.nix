let
  cardanoPkgs = import ../default.nix {};
  pkgs = cardanoPkgs.pkgs;
  runTest = testname: pkgs.runCommand "test-${testname}" { buildInputs = with cardanoPkgs; [ cardano-sl-script-runner cardano-sl-node-static cardano-sl-tools ]; } ''
    cat /etc/nsswitch.conf /etc/protocols > /dev/null
    mkdir $out
    cd $out
    mkdir poc-state
    export SCRIPT=${testname}
    testcases --configuration-file ${../lib/configuration.yaml} --db-path poc-state/db --keyfile poc-state/secret.key --log-console-off --log-config ${./log-config.yaml} --logs-prefix poc-state/logs --topology ${./topology-local.yaml} --no-brickui --policies ${../scripts/policies/policy_script-runner.yaml}
    egrep -r --color=always "Processing of proposal|We'll request data for key Tagged|Ignoring data|Proposal .* is confirmed" poc-state/relay-stdout-0
  '';
in {
  test41 = runTest "test4.1";
  test42 = runTest "test4.2";
}
