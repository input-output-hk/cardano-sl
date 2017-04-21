# Optional path for `cardano-sl`
cardano_path=${1:-../cardano-sl}

WALLET_TEST=1

./scripts/run.sh ../cardano-sl/scripts/common.sh & PIDEX=$!
$cardano_path/scripts/launch.sh & PIDNODE=$!
wait $PIDEX
wait $PIDNODE