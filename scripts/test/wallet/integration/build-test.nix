{pkgs, stdenv, walletIntegrationTests, glibcLocales }:
stdenv.mkDerivation rec {
  name = "cardano-wallet-integration-tests";
  buildInputs = with pkgs; [ jq curl glibcLocales ];
  buildCommand = ''
    ${walletIntegrationTests}
    if [ $? == 0 ]
    then
      echo $? > $out
    fi
  '';

}
