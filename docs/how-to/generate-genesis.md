# How to generate genesis

## Preparation

You need to understand, what you generate genesis for. Genesis is ought to be generated to be used on particular cluster. Nodes on this cluster will be launched with particular set of constants and you should generate genesis for this particular set.

Currently we have:

* `testnet` <- for testnet
* `testnet_staging` <- for testnet staging
* `qanet_tns` <- for qa/dev early testing
* `qanet_upd` <- for qa/dev testing of update system (~22 min epochs)

As you may see from `core/constants.yaml`:

* `testnet`, `testnet_staging`, `qanet_tns` have different `protocolMagic`
* `qanet_tns`, `qanet_upd` have same `protocolMagic`

For each different `protocolMagic` we need to generate separate genesis (because genesis has values dependent on `protocolMagic`).
Hence we have `genesisBinSuffix` set differently for all these three.

Each genesis is represented by three files:

```
core/genesis-core-{genesisBinSuffix}.bin
godtossing/genesis-godtossing-{genesisBinSuffix}.bin
genesis-info/{genesisBinSuffix}.log
```

Currently suffixes are:

* `testnet`: `tn`
* `testnet_staging`: `tns`
* `qanet_tns`/`qanet_upd`: `qa`

Files with `.bin` extension are used by cardano-sl's runtime, log file just for to be viewed by developer if there would be need to inspect how genesis was generated (it's simply output of genesis generator).

## Generation

After you decided, which set of constants to use for your cluster, you need to decide:

* Which amount of bootstrap addresses to generate (i.e. addresses to which all stake is delegated, call it `M`)
* Which amount of testnet keys to generate (keys with small balance just for testing usage, `N`)

First, you should clean your `stack` project (`stack` has problems with compilation when you change set of constants you use): `scripts/build/cardano-sl.sh -c`

Second, you should compile project in mode, you decided to use, suggest you using build helper script:

* `scripts/build/cardano-sl.sh --qa` for `qanet_tns`/`qanet_upd`
* `scripts/build/cardano-sl.sh --tns` for `testnet_staging`
* `scripts/build/cardano-sl.sh --tn` for `testnet`

Third, you should generate genesis with another helper script: `M=4 N=1200 ./scripts/generate/genesis.sh`.

Two files will be created (with different datetimes of course):

* `genesis-qanet-2017-07-16_23-51` <- folder with genesis
* `genesis-qanet-2017-07-16_23-51.tgz` <- archived folder with genesis

# Pushing data

You should upload archive on https://drive.google.com/drive/u/1/folders/0Bw1XejzMdNE6WVNzdlR6WTJaa3c

Genesis folder will contain files/dirs:

```
./genesis-core.bin
./genesisCreation.log
./genesis-godtossing.bin
./keys-fakeavvm <- seeds to generate avvm certs for testing
./keys-testnet/poor <- testnet keys
./keys-testnet/rich <- nodes keys
```

As last step you should copy:

```
./genesis-core.bin -> core/genesis-core-{genesisBinSuffix}.bin
./genesis-godtossing.bin -> godtossing/genesis-godtossing-{genesisBinSuffix}.bin
./genesisCreation.log -> genesis-info/{genesisBinSuffix}.log
```

And commit to your branch. This is it, genesis generated.
