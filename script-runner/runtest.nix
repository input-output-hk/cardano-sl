let
  cardanoPkgs = import ../default.nix {};
  pkgs = cardanoPkgs.pkgs;
  runTest = testname: pkgs.runCommand "test-${testname}" { buildInputs = with cardanoPkgs; [ cardano-sl-script-runner cardano-sl-node-static cardano-sl-tools ]; } ''
    cat /etc/nsswitch.conf /etc/protocols > /dev/null
    mkdir $out
    cd $out
    mkdir script-runner/stack-gui
    export SCRIPT=${testname}
    testcases --configuration-file ${../lib/configuration.yaml} --db-path script-runner/stack-gui/db --keyfile script-runner/stack-gui/secret.key --log-console-off --log-config ${./log-config.yaml} --logs-prefix script-runner/stack-gui/logs --topology ${./topology-local.yaml} --no-brickui --policies ${../scripts/policies/policy_script-runner.yaml}
    egrep -r --color=always "Processing of proposal|We'll request data for key Tagged|Ignoring data|Proposal .* is confirmed" script-runner/stack-gui/relay-stdout-0
  '';
in {
  test41 = runTest "test4.1";
  test42 = runTest "test4.2";
}
