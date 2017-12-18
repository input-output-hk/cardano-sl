# Cardano SL configuration

This document describes Cardano SL configuration. Note: the
description is valid for git revision **b674421cc**. It may be
outdated for newer version of the code.

## Table of contents

  * [Prerequisites](#prerequisites)
  * [Overview](#overview)
  * [Command line options](#command-line-options)
  * [Genesis](#genesis)
    + [Naming conventions](#naming-conventions)
    + [How to specify genesis](#how-to-specify-genesis)
    + [Genesis data format](#genesis-data-format)
      - [Balances and stakes](#balances-and-stakes)
    + [Genesis spec format](#genesis-spec-format)
      - [Initializer](#initializer)
      - [Balances and stakes](#balances-and-stakes-1)
      - [System start time](#system-start-time)
    + [Tools](#tools)
    + [Generating genesis for testnet](#generating-genesis-for-testnet)
    + [Generating genesis for mainnet](#generating-genesis-for-mainnet)
  * [Parts of configuration file](#parts-of-configuration-file)
    + [Core configuration besides genesis](#core-configuration-besides-genesis)
    + [Infra configuration](#infra-configuration)
    + [Update configuration](#update-configuration)
    + [Ssc configuration](#ssc-configuration)
    + [Delegation configuration](#delegation-configuration)
    + [Node configuration](#node-configuration)
  * [Predefined configurations](#predefined-configurations)
    + [Mainnet configurations](#mainnet-configurations)
      - [Using mainnet staging configuration with short epoch](#using-mainnet-staging-configuration-with-short-epoch)
    + [Internal staging configurations](#internal-staging-configurations)
    + [Devnet configuration](#devnet-configuration)
    + [Other configurations](#other-configurations)

## Prerequisites

* Read about [VSS certificates](https://cardanodocs.com/technical/pvss/#vss-certificate).
* You need to know the difference between
  [balance and stake](https://cardanodocs.com/cardano/balance-and-stake/).
* You need to know about AVVM. In short: there was vending process
  after which we have a map from AVVM address (32 bytes, base64url
  encoded) to an integer number (amount of ADA). AVVM addresses can be
  converted to Cardano-SL addresses and initially each address has
  this amount of ADA (as balance, not stake).
* [Bootstrap era and stake locking](https://cardanodocs.com/timeline/bootstrap/).
* SSC is Shared Seed Computation, it's performed by rich nodes (with
  big stake) to agree upon the seed which will be used for leaders
  selection for an epoch.

## Overview

Configuration is stored in YAML format, conventional name for the file
is `configuration.yaml`. This file contains multiple configurations,
each one is associated with a key.  An example of configuration can be
found in the file `lib/configuration.yaml`.

Configuration consists of several parts (for different parts of Cardano SL),
each will be descirbed below:

* `core` configuration
  - `genesis` configuration is the most important  part of `core`, described
in [a separate section](#genesis)
* `infra` configuration
* `update` configuration
* `ssc` configuration
* `delegation` configuration
* `node` configuration

## Command line options

Almost all executables accept several options to specify
configuration.
* `--configuration-file` can be used to specify path to configuration
file. Default value is `lib/configuration.yaml`.
* `--configuration-key` specifies key in this configuration. Default
  value is `default`.
* `--system-start` specifies system start time as unix timestamp in
  seconds. It must be provided if it's not available from
  configuration and must not be provided otherwise. More details are
  provided [below](#system-start-time).
* `--configuration-seed` can be used to specify seed used to generate
  secret data. It overrides `seed` value from `initializer` (see
  below). If _genesis data_ is used, passing `--configuration-seed` is
  prohibited and leads to a runtime error.

## Genesis

In Cardano-SL we need to specify which addresses initially have ADA
and how much. It's traditionally called _genesis block_. In Cardano-SL
we also have various parameters of the algorithm affecting consensus
along with initial balances. The hash of this data (in canonical JSON
format) is used as the parent of the very first real block.

### Naming conventions

Traditionally all initial data is called _genesis block_. However, in
Cardano-SL _genesis block_ has a different meaning: it's the first
block in an epoch, as described in Ouroboros paper. There are many
genesis blocks (one per epoch).

For this reason we call the set of all initial balances, stakes and
parameters _genesis data_ (and there is `GenesisData` type in Haskell
code). Another term is _genesis spec_ (data type in Haskell is called
`GenesisSpec`) which specifies how to create _genesis data_. See below
for more details. When we say simply _genesis_ it usually means
_genesis data_, the actual meaning is usually clear from the context.

Note: we are not trying to force everyone to change standards and say
_genesis data_ instead of _genesis block_ (though it kinda makes
sense, because in Cardano-SL _genesis data_ is not a block, it
consists of a bunch of different data). It's ok to use term _genesis
block_ and usually the meaning will be clear from the context. But for
historical reasons developers prefer to use term _genesis data_ to
refer to the initial set of parameters and balances and _genesis
block_ to refer to the first block in an epoch. It's enforced by names
in code. It may be changed later.

### How to specify genesis

Configuration is used to specify genesis data used by the node. It is
part of `core` configuration (it's accessible by key
`<configuration-key>.core.genesis`). There are two ways to specify
genesis data.

* One way is to provide genesis data itself, stored in JSON
  format. Genesis data explicitly states balances of all addresses,
  all bootstrap stakeholders, heavyweight delegation certificates,
  etc. It makes it clearer which genesis is used, but it doesn't
  describe _how_ this genesis was constructed. Also it requires more
  steps to create genesis data, than to create a spec.  This mechanism
  is used for mainnet (both production and staging) so that everyone
  can open this file and see all balances (and everything else from
  genesis data) at any time. It also will be used for testnet, at
  least for the first one.  The format is demonstrated below. `hash`
  is the hash (Blake2b 256 bits) of canonically encoded
  `mainnet-genesis.json` file. It can be computed manually using
  `scripts/js/genesis-hash.js` script.

```
    genesis:
      src:
        file: mainnet-genesis.json
        hash: 5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb
```

  The format of `mainnet-genesis.json` is described
  in [Genesis data format](#genesis-data-format).

* Another way is to provide a specification of how to generate genesis
  data. It allows one to specify how many nodes should have stake,
  which bootstrap stakeholders should be used, what should be total
  balance, etc. When node is launched with spec as genesis, it
  automatically constructs genesis data from it. This way to specify
  genesis is supposed to be used for testing clusters. It's
  questionable whether it should be used for testnet and currently we
  can't use it for testnet for performance reasons. We don't use it
  for mainnet.  See [Genesis spec format](#genesis-spec-format) for
  details.

### Genesis data format

Let's start with an example:

```
{
    "avvmDistr": {
        "0cAZzmtB2CUjlsMULb30YcBrocvK7VhQjUk--MyY2_Q=": "8333333000000",
        ...
        "zE56EfVAvv0XekVyBDGh2Iz1QT6X38YxlcBYO20hqa0=": "1773135000000"
    },
    "blockVersionData": {
        "heavyDelThd": "300000000000",
        "maxBlockSize": "2000000",
        "maxHeaderSize": "2000000",
        "maxProposalSize": "700",
        "maxTxSize": "4096",
        "mpcThd": "20000000000000",
        "scriptVersion": 0,
        "slotDuration": "20000",
        "softforkRule": {
            "initThd": "900000000000000",
            "minThd": "600000000000000",
            "thdDecrement": "50000000000000"
        },
        "txFeePolicy": {
            "multiplier": "43946000000",
            "summand": "155381000000000"
        },
        "unlockStakeEpoch": "18446744073709551615",
        "updateImplicit": "10000",
        "updateProposalThd": "100000000000000",
        "updateVoteThd": "1000000000000"
    },
    "bootStakeholders": {
        "0d916567f96b6a65d204966e6aab5fbd242e56c321833f8ba5d607da": 1,
        "4bd1884f5ce2231be8623ecf5778a9112e26514205b39ff53529e735": 1,
        "5f53e01e1366aeda8811c2a630f0e037077a7b651093d2bdc4ef7200": 1,
        "ada3ab5c69b945c33c15ca110b444aa58906bf01fcfe55d8818d9c49": 1
    },
    "ftsSeed": "736b6f766f726f64612047677572646120626f726f64612070726f766f646120",
    "nonAvvmBalances": {},
    "protocolConsts": {
        "k": 2160,
        "protocolMagic": 60987900,
        "vssMaxTTL": 6,
        "vssMinTTL": 2
    },
    "heavyDelegation": {
        "c6c7fb227037a1719b9d871ea49b6039325aeb293915da7db9620e3f": {
            "cert": "0f2ff9d1f071793dbd90fce8d47e96a09b594ee6dd00a4bac9664e4bd6af89830035ec921698b774c779eb1b6a2772d3d6ae37e630c06c75fbfecd02a6410a02",
            "delegatePk": "I07+5HIUW0Lkq0poMMzGziuILwxSyTJ4lMSsoQz4HZunD5Xsx3MfBZWc4l+206lUOFaU+spdJg7MkmFKBoVV0g==",
            "issuerPk": "AZzlT+pC5M4xG+GuRHJEXS+isikmVBorTqOyjRDGUQ+Lst9fn1zQn5OGKSXK29G2dn7R7JCugfcUebr0Dq7wPw==",
            "omega": 1
        }
    },
    "startTime": 1505621332,
    "vssCerts": {
        "0d916567f96b6a65d204966e6aab5fbd242e56c321833f8ba5d607da": {
            "expiryEpoch": 1,
            "signature": "396fe505f4287f832fd26c1eba1843a49f3d23b64d664fb3c8a2f25c8de73ce6f2f4cf37ec7fa0fee7750d1d6c55e1b07e1018ce0c6443bacdb01fb8e15f1a0f",
            "signingKey": "ohsV3RtEFD1jeOzKwNulmMRhBG2RLdFxPbcSGbkmJ+xd/2cOALSDahPlydFRjd15sH0PkPE/zTvP4iN8wJr/hA==",
            "vssKey": "WCECtpe8B/5XPefEhgg7X5veUIYH/RRcvXbz6w7MIJBwWYU="
        },
        ...
    }
}
```

Section `"avvmDistr"` contains AVVM addresses with corresponding
balances (lovelaces).

Section `"blockVersionData"` contains fundamental blockchain-related values:

*  `"heavyDelThd"` - heavyweight delegation threshold,
*  `"maxBlockSize"` - maximum size of block, in bytes,
*  `"maxHeaderSize"` - maximum size of block's header, in bytes,
*  `"maxProposalSize"` - maximum size of Cardano SL update proposal, in bytes,
*  `"maxTxSize"` - maximum size of transaction, in bytes,
*  `"mpcThd"` - threshold for participation in SSC (shared seed computation),
*  `"scriptVersion"` - script version,
*  `"slotDuration"` - slot duration, in microseconds,
*  `"softforkRule"` - rules for softfork:
   *  `"initThd"` - initial threshold, right after proposal is confirmed,
   *  `"minThd"` - minimal threshold (i.e. threshold can't become less than this one),
   *  `"thdDecrement"` - theshold will be decreased by this value after each epoch,
*  `"txFeePolicy"` - transaction fee policy's values,
*  `"unlockStakeEpoch"` - unlock stake epoch after which bootstrap era ends,
*  `"updateImplicit"` - update implicit period, in slots,
*  `"updateProposalThd"` - threshold for Cardano SL update proposal,
*  `"updateVoteThd"` - threshold for voting for Cardano SL update proposal.

Please note that values of all thresholds are multiplied by `10⁻¹⁵`. So if particular threshold is
defined as `X`, its actual value is `X * 10⁻¹⁵`.

Section `"bootStakeholders"` contains bootstrap era stakeholders'
identifiers (`StakeholderId`s) with corresponding weights.

Field `"ftsSeed"` contains seed value required for Follow-the-Satoshi
mechanism (hex-encoded).

Section `"protocolConsts"` contains basic protocol constants:

*  `"k"` - security parameter from the paper,
*  `"protocolMagic"` - protocol magic value (it's included into a
   serialized block and header and it's part of signed data, so when protocol
   magic is changed, all signatures become invalid) used to
   distinguish different networks,
*  `"vssMaxTTL"` - VSS certificates maximum timeout to live (number of epochs),
*  `"vssMinTTL"` - VSS certificates minimum timeout to live (number of epochs).

Section `"heavyDelegation"` contains an information about heavyweight delegation:

*  `"cert"` - delegation certificate,
*  `"delegatePk"` - delegate's public key,
*  `"issuerPk"` - stakeholder's (issuer's) public key,
*  `"omega"` - index of epoch the block PSK is announced in, it is
   needed for replay attack prevention, can be set to arbitrary value
   in genesis.

Keys in `heavyDelegation` dictionary are `StakeholderId`s of
certificates' issuers. They must be consistent with `issuerPk` values.

Field `"startTime"` is the timestamp of the 0-th slot.  Ideally it
should be few seconds later than the cluster actually starts. If it's
significantly later, nodes won't be doing anything for a while. If
it's slightly before the actual starts, some slots will be missed, but
it shouldn't be critical as long as less than `k` slots are missed.

Section "vssCerts" contains VSS certificates:

*  `"expiryEpoch"` - index of epoch until which (inclusive) the
   certificate is valid;
*  `"signature"` - signature of certificate,
*  `"signingKey"` - key used for signing,
*  `"vssKey"` - VSS public key.

Keys in `vssCerts` dictionary are `StakeholderId`s of
certificates' issuers. They must be consistent with `signingKey` values.

#### Balances and stakes

In _genesis data_ there are two fields which determine balances:
`avvmDistr` and `nonAvvmBalances`:
 - The former is a map from AVVM
addresses to balances
 - The latter is a map from Cardano-SL addresses
to balances

AVVM addresses can be easily converted to Cardano-SL
addresses (redeem type), so obtaining a final map from Cardano-SL
addresses to balances is trivial.

Stakes are computed from balances w.r.t. `bootStakeholders` map,
because all genesis addresses have `BootstraEra` stake
distribution.
Keys in `bootStakeholders` map are `StakeholderId`s
which will have stake and values are their weights (stake will be
distributed proportionally to those weights).

### Genesis spec format

Let's again start with an example:

```
    genesis:
      spec:
        initializer:
          testBalance:
            poors:        12
            richmen:      4
            richmenShare: 0.99
            useHDAddresses: True
            totalBalance: 600000000000000000
          fakeAvvmBalance:
            count: 10
            oneBalance: 100000
          avvmBalanceFactor: 0.5
          useHeavyDlg: True
          seed: 0
        blockVersionData:
          scriptVersion:     0
          slotDuration:      7000
          maxBlockSize:      2000000
          maxHeaderSize:     2000000
          maxTxSize:         4096 # 4 Kb
          maxProposalSize:   700 # 700 bytes
          mpcThd:            0.01 # 1% of stake
          heavyDelThd:       0.005 # 0.5% of stake
          updateVoteThd:     0.001 # 0.1% of total stake
          updateProposalThd: 0.1 # 10% of total stake
          updateImplicit:    10 # slots
          softforkRule:
            initThd:        0.9 # 90% of total stake
            minThd:         0.6 # 60% of total stake
            thdDecrement:   0.05 # 5% of total stake
          txFeePolicy:
            txSizeLinear:
              a: 155381 # absolute minimal fees per transaction
              b: 43.946 # additional minimal fees per byte of transaction size
          unlockStakeEpoch: 18446744073709551615 # last epoch (maxBound @Word64)
        protocolConstants:
          k: 2
          protocolMagic: 55550001
          vssMinTTL: 2
          vssMaxTTL: 6
        ftsSeed: "c2tvdm9yb2RhIEdndXJkYSBib3JvZGEgcHJvdm9kYSA="
        heavyDelegation: {}
        avvmDistr:
          5ZI8dniHQgyz1N/M6wQa4UDe5imEg/83a1IFmlkP6PI=: 101
```

Most values are the same as in _genesis data_. It's easier to write about
differences:
1. Genesis spec has `initializer` value not present in genesis
   data. It's described in more details [below](#initializer).
2. Thesholds are specified as floating-point numbers.
3. `ftsSeed` is base64-encoded, not hex-encoded.
4. `txFeePolicy` is specified slightly differently. The comments in
   the example explain the format pretty well. Note that the values
   are lovelaces, not ADA.
5. Addresses in `avvmDistr` are presented in base64 format, not base64(url).

#### Initializer

Sample configuration with `initializer` was presented above. It
contains the following fields:
* `testBalance`. It specifies how many nodes should be rich, how many
  nodes should be poor, how big should be total balance of rich nodes,
  total balance and whether to use HD addresses or simple PubKey
  addresses. Assignment of balances and stakes is described in
  detail [below](#balances-and-stakes).
* `fakeAvvmBalance`. It contains number of fake AVVM seeds and how
  balance of each seed. These seeds can be used to redeem ADA.
* `avvmBalanceFactor`. Real AVVM balances will be multiplied by this
  factor. Fake AVVM balances are not affected.
* `useHeavyDlg`. If it's `True`, then for each bootstrap stakeholders
  there will be a heavyweight delegation certificate from this
  stakeholder to a core node. Otherwise bootstrap stakeholders will
  participate in the protocol directly.
* `seed`. It is an integer number which will be used to generate all
  secrets (and consequenlty all public keys, addresses, etc. which are
  derived from secret keys). It can be overridden using
  `--configuration-seed` option.

Initializer makes it quite easy to generate genesis data,
because all secrets are derived from only one integer. It's suitable
for custom developer clusters because there we can let some people control all
stake. It's not true for mainnet, where we have three parties, that's
why workflow for mainnet is more complex.

Other features supported by initializer:
* It allows to have fake AVVM seeds to test ADA redemption, which are
  needed, because developers and QA specialists don't know actual
  seeds having value.
* It allows to decrease the value of all AVVM seeds using
  `avvmBalanceFactor`.
* It allows to have not only redeem addresses (derived from AVVM
  seeds), but also ready-to-use public key addresses (specified by
  `testBalance`). We can have few addresses with very big balance
  owned by us and many addresses with small balance shared with
  everybody (e.g. for faucet).

#### Balances and stakes

In _genesis spec_ there are two fields which determine balances and
stakes: `avvmDistr` and `initializer`. `avvmDistr` is supposed to
contain the result of vending (which can be found in
`lib/configuration.yaml`, see `mainnet_avvmDistr`), but it also can be
empty if you don't need AVVM addresses (or it can be modified in
a different way, however you want). Overview of `initializer` is
described [above](#initializer).

In the following, we describe how
`avvmDistr` and `nonAvvmBalances` (fields in genesis data which
determine balances and stakes) are computed from genesis spec.

* `avvmDistr` in _genesis data_ depends on `avvmDistr` from _genesis
  spec_ and also on `fakeAvvmBalance` and `avvmBalanceFactor` from `initializer`.
  We generate `fakeAvvmBalance.count`
  random seeds, convert them to AVVM addresses and assign each one
  `fakeAvvmBalance.oneBalance` coins (lovelaces). `avvmDistr` is
  multiplied by `avvmBalanceFactor` and then is merged with fake
  distribution.

  Note: if an address is in fake avvm map **and** in
  real avvm map, it likely indicates that you are doing something
  wrong, but the behavior is to pick the fake value.

* `nonAvvmBalances` in _genesis data_ depends on `testBalance`.
  Let's describe how balances and addresses generated in this case:

  - To generated balances we first find limit on how much
  coins we can generate by subtracting sum of all real avvm balances
  (considering `avvmBalanceFactor`) from maximal possible coin value
  (45000000000000000). Total generated value is the minimum between
  `testBalance.totalBalance` and the aforementioned limit.

    Let's say
    that `n` is the difference between total generated value and the sum
    of all fake avvm balances (`count * oneBalance`). Then we have
    `richmenBalance = richmenShare * n`. There will be
    `testBalance.richmen` addresses with approximately `richmenBalance /
    testBalance.richmen` coins. The rest will be evenly distributed
    among `testBalance.poors` addresses.

  - For each richman we generate a simple PubKey addresses without
    attributes.

    Poor addresses are either HD addresses with empty
    passphrase or simple PubKey addresses depending on
    `useHDAddresses` value. All addresses have BootstrapEra stake
    distribution.

    Note: rich addresses are derived from secret keys of
    richmen which may be different from bootstrap stakeholders'
    keys. It depends on `useHeavyDlg`.

#### System start time

With _genesis spec_, system start time is taken from command line option
(`--system-start`). The
reason why system start is not part
of `initializer` is that dev clusters are to be launched
often, and it's easier to change this value from CLI flags rather then configuration file.

### Tools

There are some tools relevant to genesis data.

* `cardano-keygen` has a command to dump all genesis secrets to
  files. Usage: `cardano-keygen --system-start 0 --configuration-file
  <file> --configuration-key <key> --configuration-seed <seed>
  generate-keys-by-spec --genesis-out-dir <dir>`. This command will
  generate and dump secrets to `<dir>`. To deploy a cluster you need
  keys of core nodes. They can be found in
  `<dir>/generated-keys/rich/`. It can be used only when genesis spec
  is used.

* `cardano-keygen` also has command to dump genesis data (in JSON
  format). Usage: `cardano-keygen --system-start SYSTEM_START
  --configuration-file <file> --configuration-key <key>
  --configuration-seed <seed> dump-genesis-data --path
  <file>`. Default path is `genesis-data.json`. It can be used to
  generate _genesis data_ from _genesis spec_ if you want it to be
  used by nodes or just want to verify generated data.

* `cardano-node-simple` and `cardano-node` also have command line option to
  dump genesis data. The option is
  `--dump-genesis-data-to genesis.json` (data will be dumped to
  `genesis.json`).

### Generating genesis for testnet

For testnet we can't use `spec` for genesis, at least because
converting genesis spec to genesis data is currently very slow when
there are many HD addresses (and we want to have thousands of HD
addresses in testnet). For this reason it's necessary to generate a
JSON file with genesis data first. Fortunately it can be done
automatically from genesis spec. The instruction below applies to
public testnet, staging testnet, internal staging and other clusters
when it's necessary to generate genesis data from spec.

1. You need to have a configuration which uses _genesis spec_.
   Let's assume it's in file `CONF_FILE` and its
   key is `CONF_KEY`. For instance, at the moment of writing, there is
   `internal_staging_gen` configuration which can be used to generate
   genesis for `internal_staging_full` configuration.
2. Generate very big random integer. It can be as big as you
   want. Let's call it `SEED`. You can use `python`, for example:
   `python -c "import secrets; print(secrets.randbelow(2**256))"`.
3. Figure out system start time. Ideally it should be set to exactly
   the time when nodes are launched. In practice it's impossible. If
   nodes are deployed before this time, they will just wait. There is
   nothing bad, except that start is delayed. If nodes are deployed
   after this time, some slots won't have blocks. If it's less than
   `k` slots, it shouldn't be a big problem, blockchain won't be
   dead, otherwise you should redeploy with another system
   start. Let's call it `SYSTEM_START`.
4. Run `cardano-keygen --system-start SYSTEM_START
--configuration-file CONF_FILE --configuration-key CONF_KEY
--configuration-seed CONF_SEED dump-genesis-data --path <file>`. Note:
this step may take a lot of time (very rough estimation is about 30
seconds for 1000 poor addresses).
5. After step (4) you have `<file>` with genesis data. You should put
   it into repository and change value of `file` and `hash` in
   configuration to this file and its hash (computed using
   `scripts/js/genesis-hash.js`). In this example you should update
   `internal_staging_full.core.genesis.src.{file,hash}` (updating
   `file` is not necessary if you use the same name as in the
   template).
6. You also need to know secret keys of core nodes. Use
   `cardano-keygen --system-start 0 --configuration-file CONF_FILE
   --configuration-seed SEED --configuration-key CONF_KEY
   generate-keys-by-spec --genesis-out-dir <dir>` as
   described [above](#tools). Note: you can pass arbitrary
   `system-start` here, it doesn't matter. Note: this step also can
   take a lot of time.
7. Save `SEED` somewhere and make sure it won't be lost and won't be
   known by anybody except people who maintain the testnet.

### Generating genesis for mainnet

**TODO**: we have some scripts and `README.md` in
`scripts/prepare-genesis`. We probably will never need them anymore,
but we need to preserve them to be able to review at any point for
example. Probably that `README.md` should be moved to this section or
to this folder (`docs/`). Some part of that `README.md` has been moved
to this file already.

## Parts of configuration file

### Core configuration besides genesis

**TODO**

### Infra configuration

**TODO**

### Update configuration

**TODO**

### Ssc configuration

**TODO**

### Delegation configuration

**TODO**

### Block configuration

Block configuration is stored under the `<configuration-key>.block` key.
It contains the following values:

* `networkDiameter` (seconds) — how much time it takes to propagate a
  block across the network. In practice this value determines when
  slot leaders create a block (it's done `networkDiameter` seconds
  before the end of the slot).
* `recoveryHeadersMessage` — how many headers will be sent in a
  batch. This value should be greater than `k`.

### Node configuration

Node configuration is accessible via the `<configuration-key>.node` key. It
has the following values:

**TODO**: describe values.

## Predefined configurations

This section describes various configuration keys we have in
`lib/configuration.yaml` file. For long-term clusters we also describe
differences between that cluster's configuration and mainnet
configuration. Note that generally `startTime` and `protocolMagic` are
different for all different clusters, so these differences aren't
mentioned. Also `update` configuration contains versions which are
initially set to zeros and then are increased when software/protocol
updates are performed. So `update` configurations also generally
differ and this differences isn't mentioned.

### Mainnet configurations

There are several mainnet configurations in `lib/configuration.yaml`
file for different keys.

* `mainnet_base` configuration serves as a basis for other mainnet
  configurations and shouldn't be used directly. It also can be used
  as a basis for non-mainnet configurations.
* `mainnet_full` configuration is what core nodes use in real
  mainnet. `mainnet_wallet_win64` and `mainnet_wallet_macos64` are
  almost same, but they have different `update` configurations
  (different application name, version, etc.).  They should be used by
  wallets (i. e. nodes launched with Daedalus).
* `mainnet_dryrun_full` is like `mainnet_full`, but for
  staging. `mainnet_dryrun_wallet_win64` and
  `mainnet_dryrun_wallet_macos64` are for staging wallets. Staging
  configuration is very similar to real mainnet configuration, there
  are few differences:
  - Different bootstrap stakeholders, stake is delegated to different
    public keys (number of stakeholders is the same).
  - Real mainnet has more fake addresses with 1 ADA (100 vs 6).
* `mainnet_staging_short_epoch_full` is needed for testing, because in
  real mainnet and in staging epoch is very long, but we want to be
  sure that crossing epoch boundary won't lead to problems. Cluster
  with this configuration doesn't run constantly, it's launched before
  preparing installers for release. It is very similar to
  `mainnet_dryrun_full`. There are few differences:
  - It has much smaller `k` which implies much shorter epoch.
  - It also has only 3 core nodes with different keys. Corresponding
    keys can be found in `secrets/mainnet-staging-short-epoch`
    directory.
  - It has more AVVM seeds with ADA for testing.

#### Using mainnet staging configuration with short epoch

`mainnet_staging_short_epoch_full` configuration can't be used as is,
because it has hardcoded system start time which should be modified
every time we want to launch cluster. In order to use it one needs to:

1. Figure out system start time.
2. Modify value of `startTime` in
   `mainnet-staging-short-epoch-genesis.json`.
3. Use `scripts/js/genesis-hash.hs` to compute new hash.
4. Modify value of
   `mainnet_staging_short_epoch_full.core.genesis.src.hash` in
   configuration file to the hash from the previous step.

### Internal staging configurations

`internal_staging_gen` configuration is used to generate genesis data
for `internal_staging`. Please
see [Generating genesis for testnet](#generating-genesis-for-testnet)
section for more details.

`internal_staging_full` configuration is used by full nodes in
internal staging. Currently it's a stub, because internal staging
isn't deployed yet.

`internal_staging_wallet_win64` and `internal_staging_wallet_macos64`
are like `internal_staging_full`, but for wallets.

Differences between mainnet and `internal_staging`:
* In internal staging all real AVVM balances are multiplied by `0.1`.
* In internal staging there are 100 fake AVVM addresses with 1000000
  ADA.
* In internal staging there are non-avmm balances. A few rich
  addresses and many poor addresses.

### Devnet configuration

There is a configuration called `devnet` which should be used to setup
clusters for developers or QA to test something. There are some values
which should be set carefully before using this configuration, they
depend on particular task:

* `slotDuration`, `k` are parameters of the protocol, set to
  reasonable defaults, but sometimes we may want to use different
  values. Reminder: number of slots in an epoch is `10 · k`.
* `recoveryHeadersMessage` depends on `k`. Set their values properly if
  you modify `k`. See [node configuration](#node-configuration).
* `genesis.spec.initializer.richmen` is basically
  the number of core nodes. Should be the same as the number of core
  nodes deployed.

### Other configurations

There are few more configurations which should be briefly
mentioned. `dev` configuration is used by developers to launch cluster
locally. It's very simple and provides bare minimum. `bench`
configuration is used for benchmarks. `test` configuration is used in
tests by default (it's embedded into binary). `default` configuration
is an alias for `dev`.
