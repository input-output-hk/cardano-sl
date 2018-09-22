{ runCommand, srcroot, cabal-merger, lib }:

let
  justCabals = name: type: (type == "directory" || lib.hasSuffix ".cabal" name);
  filteredRoot = lib.cleanSourceWith { filter = justCabals; src = srcroot; };
in runCommand "everything.cabal" { buildInputs = [ cabal-merger ]; } ''
cp -r ${filteredRoot} src
cd src
chmod -Rv +w .
cabal-merger \
  ./acid-state-exts/acid-state-exts.cabal \
  ./binary/cardano-sl-binary.cabal \
  ./binary/test/cardano-sl-binary-test.cabal \
  ./chain/cardano-sl-chain.cabal \
  ./chain/test/cardano-sl-chain-test.cabal \
  ./client/cardano-sl-client.cabal \
  ./core/cardano-sl-core.cabal \
  ./core/test/cardano-sl-core-test.cabal \
  ./crypto/cardano-sl-crypto.cabal \
  ./crypto/test/cardano-sl-crypto-test.cabal \
  ./db/cardano-sl-db.cabal  \
  ./generator/cardano-sl-generator.cabal \
  ./infra/cardano-sl-infra.cabal  \
  ./lib/cardano-sl.cabal \
  ./networking/cardano-sl-networking.cabal \
  ./node-ipc/cardano-sl-node-ipc.cabal \
  ./tools/cardano-sl-tools.cabal \
  ./util/cardano-sl-util.cabal \
  ./util/test/cardano-sl-util-test.cabal \
  ./utxo/utxo.cabal \
  ./wallet-new/cardano-sl-wallet-new.cabal \
  ./wallet/cardano-sl-wallet.cabal \
  ./x509/cardano-sl-x509.cabal
cp output $out
''
