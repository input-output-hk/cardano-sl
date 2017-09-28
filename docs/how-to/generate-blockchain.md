# How to Generate Blockchain

It is possible situation when blockchain is not advanced for more than `k` slots.
In this case we should generate it. Use `cardano-block-gen` command for it.

Basic example of the command:

```
$ stack exec -- cardano-block-gen                \
    --system-start 0                             \
    --configuration-file node/configuration.yaml \
    --configuration-key mainnet_full             \
    --blocks 1000                                \
    --generated-db some-db                       \
    --secret PATH_TO_SECRET_KEY                  \
    --tx-count "(0,0)"                           \
    --append
```

where:

*  `1000` is a length of generated blockchain (number of blocks),
*  `some-db` is a path to generated database.
*  `PATH_TO_SECRET_KEY` is the path to a secret key we're using for generating blockchain.
   This parameter can be specified **multiple** times. Please note that during Bootstrap era
   we should use secret key Bootstrap stakeholders delegated heavily to.

It is possible to use a custom seed for generating blocks. Use `--seed` parameter
to define integer number that will be used as a seed, for example `--seed 100`.
If the value of seed is not defined, random integer number will be used instead.
