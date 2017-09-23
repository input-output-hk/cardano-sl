# Genesis Preparation

This document describes the steps should be taken to prepare genesis block which is
required for launching a cluster, for example Mainnet.

These are the steps we have to perform to do it:

0.  preparation (everyone),
1.  delegate keys (IOHK DevOps),
2.  stakeholder keys and delegate certificates (each stakeholder independently),
3.  finalize AVVM utxo (IOHK DevOps),
4.  VSS certificates (IOHK DevOps),
5.  Genesis JSON: construction and validation (IOHK DevOps and Cardano SL Core).

Responsible are defined in the parentheses.

Please note that **parties** for Mainnet are IOHK, SGG and CF.

## 0. Preparation

First of all, we have to build Cardano SL using this script:

```
bash ./bootstrap-cardano.sh 2>&1 | tee bootstrap.log./build-cardano.sh
```

It will install Nix package manager and build Cardano SL (using proper branch)
inside of the Nix environment.

## 1. Delegate Keys

Then we have to generate keys for the cluster's nodes using this script:

```
DELEGATE_PUBLIC=<pulic out path> REPO_PATH=<path to repo> DELEGATE_SECRET=<full path to node secret keys> bash <path to repo>/scripts/prepare-genesis/gen-delegate-keys.sh 2>&1 | tee step1.log
```

This script will generate 7 secret keys, for 7 nodes in the Mainnet cluster.

Generated secret keys will be stored in `cardano-sl/keys/delegate/` directory.
Each secret key should be put in the secret storage of IOHK and later should be
uploaded to the node and used by it to perform blockchain-related procedures (for
example, to sign new blocks).

This script will also show an information about generated keys. For example:

```
[keygen:INFO] Processing command
[keygen:INFO] Primary: 9645e94f800a5dbcf2ea4f21c60755ca4b75ba7efe02ea20a8278d1e2ff185f1c22cd797a8c5e131a40906f89d57fcaea6c70dcdf3869e4a6a7a6a04292ba10f; address hash: 204779e68379f19ad88c3599c22015988febb4c0258f100a61346ce1
[keygen:INFO] No wallet set
[keygen:INFO] Keys:
[keygen:INFO] Vss PK: 582103803acf1550b381926d748949d5bd4d8a7b7cfe1f8112f52555d49ff88ac6db77; short = vsspub:6cc4ca60
```

`Primary` line contains public key as well as address hash. All these public keys
should be published in any public channel: GitHub Gist, website, etc. 

## 2. Stakeholder Keys and Delegate Certificates

Now stakeholders' keys should be generated. We have 7 nodes, so we need 7
stakeholders' keys as well. Please note that purpose of these keys is
**different** than keys generated in step `1`. These keys will be used for
heavyweight delegation.

In addition, we have to generates delegation certificates for stakeholders. Such a
certificate allows to delegate stake from `SK` to `DPK`, where `SK` is a stakeholder's
secret key and `DPK` is a delegate's (node's) public key (generated in step `1`).

Since we have 7 Bootstrap stakeholders and 7 nodes, we need 7 delegation certificates.
So these delegation certificates will do mapping between particular node's key and
stakeholder's key.

Please note that each of parties should generate its own stakeholders' secret keys and
certificates. Thus, IOHK nominated individuals should execute this script:

```
STAKEHOLDER_NAME=iohk STAKEHOLDER_PUBLIC=<public ouput dir> STAKEHOLDER_SECRET=<iohk secret keys dir> DELEGATE_PUBS=<path to list of all dpks> REPO_PATH=<path-to-repo> bash <path-to-repo>/scripts/prepare-genesis/keys-and-certs.sh  2>&1 | tee step2-iohk.log
```

CGG nominated individuals team should execute this one:

```
STAKEHOLDER_NAME=cgg STAKEHOLDER_PUBLIC=<public ouput dir> STAKEHOLDER_SECRET=<cgg secret keys dir> DELEGATE_PUBS=<path to list of all dpks> REPO_PATH=<path-to-repo> bash <path-to-repo>/scripts/prepare-genesis/cgg-keys-and-certs.sh  2>&1 | tee step2-cgg.log
```

And CF nominated individuals - this one:

```
STAKEHOLDER_NAME=cf STAKEHOLDER_PUBLIC=<public ouput dir> STAKEHOLDER_SECRET=<cf secret keys dir> DELEGATE_PUBS=<path to list of all dpks> REPO_PATH=<path-to-repo> bash <path-to-repo>/scripts/prepare-genesis/cf-keys-and-certs.sh  2>&1 | tee step2-cf.log
```

Generated secret keys will be stored in `cardano-sl/keys/stakeholder/` directory.

Each stakeholder's secret key should be put in the secret storage of the party. Please node
that the secret storage here should be **offline**, whereas the node keys (from previous step)
have to be kept online on the operational servers.

Then delegation certificate and stakeholder's (issuer's) public key should be published in any
public channel: GitHub Gist, website, etc. Later these delegation certificate and stakeholder's
identifier (address hash) will be included in the `"heavyDelegation"` section in genesis JSON.

## 3. Genesis JSON: construction and validation

Now genesis JSON should be prepared. Eventually this JSON is required to build genesis block for Mainnet.
And at this point IOHK DevOps team has all the data required for genesis JSON preparation.

The file `genesis-mainnet.json` contains detailed instructions how to generate genesis block.
It contains balances, keys, certificates, etc. So the hash of `genesis-mainnet.json` will be a
genesis hash. File `genesis-mainnet.json` is a part of Cardano SL repository.

### 3.1. Genesis JSON Format

Example of `genesis-mainnet.json`:

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

### 3.2. Genesis JSON Format Explanation

Section `"avvmDistr"` contains AVVM keys with corresponding coins values.

Section `"blockVersionData"` contains fundamental blockchain-related values:

*  `"heavyDelThd"` - heavyweight delegation threshold,
*  `"maxBlockSize"` - maximum size of block, in bytes,
*  `"maxHeaderSize"` - maximum size of block's header, in bytes,
*  `"maxProposalSize"` - maximum size of Cardano SL update proposal, in bytes,
*  `"maxTxSize"` - maximum size of transaction, in bytes,
*  `"mpcThd"` - threshold for participation in MPC,
*  `"scriptVersion"` - script version, 
*  `"slotDuration"` - slot duration, in microseconds,
*  `"softforkRule"` - rules for softfork:
   *  `"initThd"` - initial threshold, right after proposal is confirmed,
   *  `"minThd"` - minimal threshold (i.e. threshold can't become less than this one),
   *  `"thdDecrement"` - theshold will be decreased by this value after each epoch,
*  `"txFeePolicy"` - transaction fee policy's values,
*  `"unlockStakeEpoch"` - unlock stake epoch,
*  `"updateImplicit"` - update implicit period, in slots,
*  `"updateProposalThd"` - threshold for Cardano SL update proposal,
*  `"updateVoteThd"` - threshold for voting for Cardano SL update proposal.

Please note that values of all thresholds are fractional numbers here. So if particular threshold is
defined as `X`, it's actual value is `X * 10^-15`.

Section `"bootStakeholders"` contains Bootstrap stakeholders' identifiers (see step `2`) with
corresponding weight.

Field `"ftsSeed"` contains seed value required for Follow-the-Satoshi mechanism.

Section `"protocolConsts"` contains basic protocol constants:

*  `"k"` - security parameter from the paper,
*  `"protocolMagic"` - protocol magic value, 
*  `"vssMaxTTL"` - VSS certificates maximum timeout to live (number of epochs),
*  `"vssMinTTL"` - VSS certificates minimum timeout to live (number of epochs).

Section `"heavyDelegation"` contains an information about heavyweight delegation:

*  `"cert"` - delegation certificate,
*  `"delegatePk"` - delegate's public key,
*  `"issuerPk"` - stakeholder's (issuer's) public key,
*  `"omega"` - index of epoch the block PSK is announced in, it is needed for replay attack prevention.

Field `"startTime"` contains timestamp for the proper start of the cluster.
IOHK DevOps team should provide proper start time.

Section "vssCerts" contains VSS certificates:

*  `"expiryEpoch"` - index of epoch when this certificate will expire,
*  `"signature"` - signature of certificate,
*  `"signingKey"` - key used for signing,
*  `"vssKey"` - VSS public key (can be read from node's secret key, as shown in step `1`).

### 3.3. Genesis JSON formation

After step 1 and step 2 are executed, public information (all data from directories `DELEGATE_PUBLIC` and `STAKEHOLDER_PUBLIC` should be gathered).

* `${DELEGATE_PUBLIC}/vss.json` should be copied to `vssCerts`
* `${STAKEHOLDER_PUBLIC}/dpks.json` from all stakeholders should be merged and merged JSON to be set as `heavyDelegation`
* `${STAKEHOLDER_PUBLIC}/pubs.txt` from all stakeholders should be merged, assigned appropriate weights and set as `bootStakeholders`

Also Devops should:
* Set appropriate `startTime`
* Set appropriate `protocolMagic`

### 3.4. Genesis JSON Validation

Please note that after `genesis-mainnet.json` will be prepared, it must be validated.

**Responsible**: Jeremy Wood, Duncan Coutts, George Agapov.
