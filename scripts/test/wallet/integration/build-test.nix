{pkgs, stdenv, walletIntegrationTests }:
stdenv.mkDerivation rec {
  name = "cardano-wallet-integration-tests";
  buildInputs = with pkgs; [ jq curl ];
  buildCommand = ''
    ${walletIntegrationTests}
    if [ $? == 0 ]
    then
      echo $? > $out
    fi
  '';

}
