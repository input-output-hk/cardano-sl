{ runCommand, stdenv
, nixTools, cardanoConfig
, version ? "unstable", gitrev, buildId ? null }:

with stdenv.lib;

let
  cardanoWallet = nixTools.nix-tools.exes.cardano-wallet;
  cardanoTools = nixTools.nix-tools.exes.cardano-sl-tools;

in runCommand "cardano-daedalus-bridge-${version}" {
  inherit version gitrev buildId;
} ''
  # Generate daedalus-bridge
  mkdir -p $out/bin
  cd $out
  ${optionalString (buildId != null) "echo ${buildId} > build-id"}
  echo ${gitrev} > commit-id
  # this comes from cardano-sl.cabal, via an inherit in default.nix
  echo ${version} > version

  cp --no-preserve=mode -R ${cardanoConfig}/lib config
  cp ${cardanoConfig}/log-configs/daedalus.yaml $out/config/log-config-prod.yaml
  cp ${cardanoTools}/bin/cardano-launcher* bin/
  cp ${cardanoTools}/bin/wallet-extractor* bin/
  cp ${cardanoTools}/bin/cardano-x509-certificates* bin/
  cp ${cardanoWallet}/bin/cardano-node* bin/

  ${optionalString (stdenv.targetPlatform.libc != "msvcrt") ''
    # test that binaries exit with 0
    ./bin/cardano-node --help > /dev/null
    HOME=$TMP ./bin/cardano-launcher --help > /dev/null
  ''}
''
