{stdenv, jq, curl, runCommand, walletIntegrationTests, glibcLocales }:
runCommand "cardano-wallet-integration-tests" {
  buildInputs = [ jq curl glibcLocales ];
}
''
  #!${stdenv.shell}
  function capture_logs {
    echo "The build failed with exit code $?"
    mkdir -pv $out/nix-support
    touch $out/nix-support/failed
    tar -czvf  $out/logs.tar.gz state-demo/logs
    echo "file binary-dist $out/logs.tar.gz" >> $out/nix-support/hydra-build-products
    exit 0
  }
  ${walletIntegrationTests} || capture_logs
  touch $out
  exit 0
''
