# How to launch a testnet

This document explains how the Cardano SL public testnet was launched
and the configuration settings used.

Related documents:

 * [`how-to/generate-genesis.md`](./generate-genesis.md)
 * [`how-to/connect-to-cluster.md`](./connect-to-cluster.md)
 * [`configuration.md`](../configuration.md)

## Genesis Data

A new genesis block is created for the testnet. The parameters used to
generate genesis data are specified in the `testnet_launch` section of
[`lib/configuration.yaml`](../../lib/configuration.yaml).

All values are inherited from the `mainnet_base` section, unless
overridden.

### Initializer

 * `protocolMagic` -- this number is different from mainnet's magic to
   ensure that signatures from testnet are different to mainnet.
 * `requiresNetworkMagic` -- `NMMustBeJust` (the default) -- will
   ensure that the format of addresses and hence transactions are
   different and incompatible to those of mainnet.
 * `totalBalance` -- 42,000,000,000 Ada -- is close to the maximum
   possible coin value (45B Ada).
 * `avvmDistr` -- this is the empty hashmap, unlike in mainnet which
   has many Ada redemption addresses.
 * `fakeAvvmBalances` -- there are 100 fake AVVM seeds which can be
   used to redeem 20,000,000 Ada each on the testnet.
 * `poors` -- Unlike mainnet which has none, there are 100 "poor"
   nodes with generated keys.
 * `richmen` -- There are 7 testnet richmen, i.e. core nodes, same as
   mainnet.
 * `richmenShare` -- 95%. After subtracting the total balance of fake
   AVVM certificates, there will be 40,000,000,000 Ada divided between
   the 7 rich and 100 poor nodes. A single poor node will recieve
   0.05% of that total, which is 20,000,000 Ada.

### `blockVersionData`

The mainnet `blockVersionData` has been updated since its genesis. The
testnet will be launched with these new parameters matching mainnet's
current `blockVersionData`.

| `blockVersionData` | Value | Description                                |
| :----------------- | ----: | :------------------------------------------|
| `maxHeaderSize`    | 2KB   | Maximum size of block's header             |
| `maxProposalSize`  | 70KB  | Maximum size of Cardano SL update proposal |
| `maxTxSize`        | 64KiB | Maximum size of transaction                |

### Update system block version

In addition, the `update.lastKnownBlockVersion` is set at `0.0.0` in
the `testnet_full`. The nodes will refuse to create blocks if this
value is higher than zero.
 
## Faucet

The testnet faucet will dispense a random amount in the range of 500
to 1500 Ada.

It is not rate limited, but automatic withdrawals are prevented with
[reCAPTCHA](https://developers.google.com/recaptcha/).

According to the parameters above, up to 2,000,000,000 Ada could be
dispensed, though not all should be made available to the faucet.

## Maintaining control of testnet Ada

IOHK will retain control of the majority of testnet Ada to minimise
the risk of the testnet becoming a threat blockchain and the testnet
currency assuming value.

The core nodes (richmen) control 95% of stake.

Additionally, not all AVVM certificates and "poor" addresses should be
sent to the faucet.

## Redeeming ADA from fake AVVM seeds

The certificate seeds are in the `keys-fakeavvm` directory within the
output of `cardano-keygen generate-keys-by-spec`.

Copy one of `fake-N.seed` to the clipboard, then use the Daedalus
wallet UI to redeem the testnet Ada.

1. Click on the _Daedalus_ → _Ada redemption_ menu
2. Go to the _Regular_ tab
3. Paste the seed into the _Redemption key_ box.

## Importing "poor" secret keys into a wallet

Copy one of `generated-keys/poor/keyN.sk` to a secure location and
then use the v0 `/api/wallets/keys` endpoint to import it into a
wallet.

## Launching a demo cluster with testnet genesis

This script will generate genesis data using the procedure described
in [`configuration.md`](../configuration.md), and then launch a demo
cluster with that genesis data.

    nix-build -A demoClusterLaunchGenesis -o demo-cluster-launch-genesis.sh
    ./demo-cluster-launch-genesis.sh
    
## Connecting to the testnet

This script will connect to the running public testnet.

     nix-build -A connectScripts.testnet.wallet -o connect-testnet-wallet.sh
    ./connect-testnet-wallet.sh
