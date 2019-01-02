with import ../../../lib.nix;

{ stdenv, writeScript
, jq, coreutils, curl, gnused, openssl, time, haskellPackages

, cardano-sl-tools, cardano-sl-wallet, cardano-sl-wallet-tool

, environment ? "mainnet"
}:

let
  cardanoDeps = [ cardano-sl-tools cardano-sl-wallet cardano-sl-wallet-tool ];
  testRunnerDeps = [ jq coreutils curl gnused openssl time haskellPackages.hp2pretty ];
  allDeps =  testRunnerDeps ++ cardanoDeps;

in
  writeScript "acceptance-tests-${environment}" ''
    #!${stdenv.shell}
    export PATH=${makeBinPath allDeps}:$PATH

    ${utf8LocaleSetting}
    ${time}/bin/time -v cardano-sl-acceptance-tests ${environment}

    hp2pretty cardano-node.hp
    sync-plot sync sync-stats.json sync-stats.png
    sync-plot restore restore-stats.json restore-stats.png

    if [ -n "$BUILDKITE" ]; then
      buildkite-agent artifact upload sync-stats.png
      buildkite-agent artifact upload restore-stats.png
      buildkite-agent artifact upload cardano-node.hp
      buildkite-agent artifact upload cardano-node.svg
      echo "+++ Heap profile"
      printf '\033]1338;url='"artifact://cardano-node.svg"';alt='"Heap profile"'\a\n'
      echo "+++ Charts for ${environment}"
      printf '\033]1338;url='"artifact://sync-stats.png"';alt='"Blockchain sync"'\a\n'
      printf '\033]1338;url='"artifact://restore-stats.png"';alt='"Wallet restore"'\a\n'
    fi
  ''
