# How to Generate Blockchain

It is possible situation when blockchain is not advanced for more than `k` slots.
In this case we should generate it. Use `cardano-block-gen` command for it.

## Basic Command

The simplest example of the command:

```
$ stack exec -- cardano-block-gen   \
    --system-start 0                \
    --blocks 1000                   \
    --generated-db new-db
```

where:

*  `1000` is a length of generated blockchain (number of blocks),
*  `new-db` is a path to generated database.

## Additional Parameters

If the database already exists, we can use `--append` option. In this case new
database won't be created and records will be appended to existing database.

It is possible to use a custom seed for generating blocks. Use `--seed` parameter
to define integer number that will be used as a seed, for example `--seed 100`.
If the value of seed is not defined, random integer number will be used instead.

Since we generate blocks with random transactions it can be useful to limit the
number of outputs in these transactions. Use `--tx-max-outs` parameter for it,
for example `--tx-max-outs 10`.
