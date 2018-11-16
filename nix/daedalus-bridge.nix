{ runCommand, stdenv
, cardano-sl-node, cardano-sl-tools, cardano-wallet, cardano-sl-config
, version, gitrev, buildId ? null }:

with stdenv.lib;

runCommand "cardano-daedalus-bridge-${version}" {
  inherit version gitrev buildId;
} ''
  # Generate daedalus-bridge
  mkdir -p $out/bin
  cd $out
  ${optionalString (buildId != null) "echo ${buildId} > build-id"}
  echo ${gitrev} > commit-id
  echo ${version} > version

  cp --no-preserve=mode -R ${cardano-sl-config}/lib config
  cp ${cardano-sl-config}/log-configs/daedalus.yaml $out/config/log-config-prod.yaml
  cp ${cardano-sl-tools}/bin/cardano-launcher bin
  cp ${cardano-sl-tools}/bin/cardano-x509-certificates bin
  cp ${cardano-wallet}/bin/cardano-node bin

  # test that binaries exit with 0
  ./bin/cardano-node --help > /dev/null
  HOME=$TMP ./bin/cardano-launcher --help > /dev/null
''
