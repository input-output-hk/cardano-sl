{pkgs, stdenv, walletIntegrationTests, glibcLocales }:
stdenv.mkDerivation rec {
  name = "cardano-wallet-integration-tests";
  buildInputs = with pkgs; [ jq curl glibcLocales ];
  buildCommand = ''
    ${walletIntegrationTests}
    EXIT_CODE=$?
    mkdir -pv $out/nix-support
    if [ $EXIT_CODE != 0 ]
    then
      touch $out/nix-support/failed
      tar -czvf  $out/logs.tar.gz state-demo/logs
      echo "file binary-dist $out/logs.tar.gz" >> $out/nix-support/hydra-build-products
    fi
  '';

}
