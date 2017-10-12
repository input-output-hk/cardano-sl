# How to Rollback Blocks

It's possible when some broken data appear on the blockchain. In this case we
have to rollback some blocks. Tool `cardano-auxx` provides such a functionality.

Example of the command:

```
$ stack exec -- cardano-auxx                      \
    --system-start 0                              \
    --configuration-file node/configuration.yaml  \
    --configuration-key mainnet_full              \
    --log-config log-config-prod.yaml             \
    --logs-prefix "logs/mainnet-1.0"              \
    --db-path db-mainnet-1.0                      \
    cmd --commands="rollback 5 /tmp/cardano-sl-rollback-txs-dump"
```

This command will rollback 5 last blocks from the DB and dump transactions from these
blocks in `/tmp/cardano-sl-rollback-txs-dump` binary file.

Please note that if the number of blocks will be greater than the current number of blocks
in the blockchain, this command will rollback as much blocks as possible **except** 0-th
genesis block! It is **illegal** to rollback 0-th genesis block, so if blockchain contains 50
blocks, it is possible to rollback only 49 ones.

Binary dump with transactions can be used later for sending these transactions in the network
ony by one, using `send-from-file` subcommand.
