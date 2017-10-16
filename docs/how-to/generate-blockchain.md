# How to Generate Blockchain

It is possible situation when blockchain is not advanced for more than `k` slots.
In this case we should generate it. Use `cardano-auxx` and command `generate-blocks` for it.

Launch `cardano-auxx`:

```
$ stack exec -- cardano-auxx                     \
    --system-start 0                             \
    --configuration-file node/configuration.yaml \
    --configuration-key mainnet_full             \
    --db-path some-db                            \
```

Then ensure you have all the needed keys imported (see `add-key` and 
`add-key-pool` commands). Next is running `generate-blocks <N> <seed>?` command, where
`<N>` is number of blocks to generate and `<seed>` is optional generation seed.
Transaction payload generation is disabled by default.
