mkdir -p logs/rc0

stack exec -- cardano-node \
  --peer 35.157.26.73:3000/dYGuDj0BrJxCsTC9ntJE7ePT7wUoVdQMH3sKLzQD8bo= \
  --flat-distr '(20,60000000)' \
  --spending-genesis 5 \
  --vss-genesis 5 \
  --wallet \
  --listen 0.0.0.0:3001 \
  --rebuild-db \
  --wallet-db-path wallet-db/ \
  --json-log=logs/rc0/node0.json \
  --logs-prefix logs/rc0 \
  --log-config logs.yaml
