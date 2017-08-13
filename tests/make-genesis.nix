{ cardano-sl-tools, runCommand }:

runCommand "genesis" { buildInputs = [ cardano-sl-tools ]; } ''
  mkdir $out
  cardano-keygen generate-genesis --genesis-dir $out -m 5 -n 12000 --richmen-share 0.94 --testnet-stake 19072918462000000 --utxo-file ${../scripts/avvm-files/utxo-dump-last-new.json} --blacklisted ${../scripts/avvm-files/full_blacklist.js} --fake-avvm-entries 100 |& tee $out/genesisCreation.log
''
