# Transaction processing

## Table of contents

  * [Prerequisites](#prerequisites)
  * [Overview](#overview)
  * [Global transaction processing](#global-transaction-processing)
    + [GState](#gstate)
    + [Unknown data handling](#unknown-data-handling)
    + [Verification](#verification)
      - [General checks](#general-checks)
      - [Outputs checks](#outputs-checks)
      - [Inputs checks](#inputs-checks)
        * [Witness checks](#witness-checks)
          + [Public key witness](#public-key-witness)
          + [Script witness](#script-witness)
          + [Redeem witness](#redeem-witness)
          + [Unknown witness](#unknown-witness)
        * [Sums check](#sums-check)
        * [Transaction fee check](#transaction-fee-check)
          + [Size-linear policy](#size-linear-policy)
    + [GState modification](#gstate-modification)
      - [UTXO modification](#utxo-modification)
      - [Computing stakes for transaction output](#computing-stakes-for-transaction-output)
      - [Stakes modification](#stakes-modification)
  * [Local transaction processing](#local-transaction-processing)

## Prerequisites

* You should know what a transaction is, its structure and about tx
  witnesses. You can read about it
  [here](https://cardanodocs.com/cardano/transactions/).
* You also should know technical details about
  [addresses](https://cardanodocs.com/cardano/addresses/).
* Please read about
  [balances and stakes](https://cardanodocs.com/cardano/balance-and-stake/) and
  don't confuse them.
* [Bootstrap era](https://cardanodocs.com/timeline/bootstrap/).
* [Block structure](https://cardanodocs.com/technical/blocks/#design).
  Specifically, you need to know about how transactions are stored in blocks.
* [Update Mechanism](https://cardanodocs.com/cardano/update-mechanism/).

## Overview

Transaction processing consists of two parts: global and local. Global
transaction processing is a part of block processing which checks
whether transactional payload of blocks is valid and updates
corresponding state when blocks are applied/rolled back. Local
transaction processing checks standalone transactions and updates
local mempool. These two parts have almost same logic with a few
differences. We first describe global transaction processing and then
describe how local transaction processing differs from it.

Transaction processing is abbreviated to `txp` in code and some other
places.

## Global transaction processing

Global transaction processing can be described by presenting an
algorithm to solve the following problem:

> Given a sequence of blocks `B₀, B₁, B₂, …` (where `B₀` is the first
genesis block) check whether transactions payload (`TxPayload`) from
these blocks are valid.
[Recall](https://cardanodocs.com/technical/blocks/#main-block)
that transactions payload contains actual transactions (stored in the
Merkle tree) and list of witnesses for inputs of those transactions.

The algorithm is similar to the one from Bitcoin (for example, UTXO is used to prevent
double-spending), but is more complicated due to various reasons, including:

* Extendable (via update mechanism) data structures
* Maintaining stakes in addition to balances


We describe global txp as stateful algorithm:
* Initial state `S₀` is derived from blockchain genesis data (see [mainnet genesis JSON](https://raw.githubusercontent.com/input-output-hk/cardano-sl/e7cfb1724024e0db2f25ddd2eb8f8f17c0bc497f/node/mainnet-genesis.json))
* `S₁, S₂, …` are maintained as sequential application of blocks  `B₀, B₁, B₂, … ` to state `S₀`
* State transition function. Given GState `S` and a main block `B` return either an error describing why `B` is invalid (w.r.t. tx payload) or new GState `S'`
  ```
  verifyAndApplyGState :: GState -> MainBlock -> Either TxPayloadVerificationError GState
  -- ^ Note, that function definition and types are different in code and are put here for reader's convenience
  ```
  * Note, that state transition function is defined only for main blocks. This is done because genesis blocks (as defined in Ouroboros) don't have tx payload and thus are not considered by global txp.
  * Recall that tx payload contains transactions (stored in Merkle tree) and their witnesses.

Note, that in theory transaction verification can be stateless (except genesis
state `S₀`) and be described without mentioning any additional state
(apart from blocks themselves). But in practice it would be very
inefficient and inconvenient.

For sake of simplicity, we describe state transition function in two parts:
* Verification: given GState `S` and a main block `B`, check whether `B` is valid (and can be applied to `S`)
* Modification: given GState `S` and a main block `B` (successfully verified against `S`), produce `S'`

### GState

In this section we describe parts of GState relevant to transaction
processing (GState is an actual structure in code used by few more
components).

* UTXO (unspent transaction outputs)
  ```
  type TxId = Hash Tx
  data TxOut = TxOut
     { txOutAddress :: !Address
     , txOutValue   :: !Coin
     }
  data TxIn
     = TxInUtxo
     { -- | Which transaction's output is used
       txInHash  :: !TxId
       -- | Index of the output in transaction's outputs
     , txInIndex :: !Word32
     }
     | TxInUnknown !Word8 !ByteString

  data TxOutAux = TxOutAux
     { toaOut   :: !TxOut
     -- ^ Tx output
     }


  type Utxo = Map TxIn TxOutAux
  ```

  UTXO is a map from `TxIn` to
  `TxOutAux` ([code](https://github.com/input-output-hk/cardano-sl/blob/e7cfb1724024e0db2f25ddd2eb8f8f17c0bc497f/txp/Pos/Txp/Toil/Types.hs#L58)) which contains all unspent outputs.
  * `TxOutAux` is just an alias for `TxOut` ([code](https://github.com/input-output-hk/cardano-sl/blob/e7cfb1724024e0db2f25ddd2eb8f8f17c0bc497f/txp/Pos/Txp/Core/Types.hs#L160)). Later it can be extended if we want to associate
  more data with unspent outputs (e. g. slot of the block in which
  this transaction appeared).
  * If a transaction `A` has 1 output (`out1`) which hasn't been
  spent yet, UTXO will have a pair `(TxIn (hash A) 0, out1)`.

* Stakes
  ```
  type StakesMap = HashMap StakeholderId Coin
  type TotalStake = Coin -- this type doesn't exist in code and put here for convenience of reader
  ```
  * Stakes are used by other parts of the protocol (for example, leader election
  process Follow-the-satoshi or FTS)
  * Stakes are not needed for tx verification
  * Stakes are maintained by tx processing: we store total stake and a map from `StakeholderId` to its stake (`Coin`)
* Adopted `BlockVersionData`.
  This data type ([code](https://github.com/input-output-hk/cardano-sl/blob/e7cfb1724024e0db2f25ddd2eb8f8f17c0bc497f/core/Pos/Core/Types.hs#L318)) contains various
  parameters of the algorithm
  which can be updated by Update System (details are not covered in this document).

  Txp doesn't modify this value, it only reads it.

  Some values are relevant for transaction processing:
  * `maxTxSize` – maximal size of a transaction.
  * `scriptVersion` – determines the rules which should be used to
    verify scripts.
  * `txFeePolicy` – determines the rules to check whether a
    transaction has enough fees.
  * `unlockStakeEpoch` – epoch when bootstrap era ends.

### Unknown data handling

Many types in Cardano-SL are designed in an extensible way. They can
be extended via softfork in future (e. g. new data can be attached or
semantics can be changed). Examples are:
* `TxIn` has two constructors `TxInUtxo` and `TxInUnknown`. The former
  is just a reference to an unspent output, it's the only `TxIn` type
  we use currently. `TxInUnknown` contains arbitrary bytestring as
  payload. Later we may introduce another type of transaction inputs
  (e. g. to redeem fees). Then old software will parse these inputs
  as `TxInUnknown`, but new software will parse them as new type of
  input (and process them appropriately).
* `Attributes` is basically a map from 1-byte integers to arbitrary
  values. Some values can be parsed into known data types, other
  values are treated as _unparsed fields_. `Attributes` are part of
  various data types, e. g. `Tx`. In version 0 `Tx` always has empty
  attributes. But in version 1 we may add more data to `Tx` (put it
  into `Attributes`). In this case new software will parse this data
  and old software will treat it as unparsed fields.
* Unknown address type. Each valid address in Cardano-SL has a
  type. It can be one of fixed address types or unknown address type
  which can't be interpreted by current version of software.

There are few more examples, but these three should demonstrate the
general idea. If we encounter unknown data (unparsed fields, unknown
tx input type, unknown address type, etc.) in a block, there are two
possible behaviours:
1. Consider such block invalid.
2. Do as many checks as we can and ignore checks which can't be done
   because data is unknown. This behaviour depends on which type of
   data we are processing, each particular case is described in more
   details below.

The behaviour depends on two protocol versions: version used by this
software and last adopted version. We verify that data in blocks is
known if protocol version used by this software is greater than or
equal to the adopted version. That's because in this case:

1. Authors of this software are aware of the adopted version.
2. Each issued block must be formed with respect to adopted version.

Comparison is quite tricky here. Table below demonstrates it.

| Our   | Adopted | Check? |
| ----- | ------- | ------ |
| 1.2.3 |  1.2.3  | Yes    |
| 1.2.3 |  1.2.4  | No     |
| 1.2.3 |  1.2.2  | No     |
| 1.2.3 |  1.3.2  | No     |
| 1.2.3 |  1.1.1  | Yes    |
| 2.2.8 |  1.9.9  | Yes    |

If `(major, minor)` of our version is greater than of adopted
one, then check is certainly done. If it's equal, then check is
done only if `alt` component is the same as adopted one. In
other cases (i. e. when our `(major, minor)` is less than from
adopted version) check is not done.

Note: when we say that attributes must be known, it means that
unparsed fields must be empty.

### Verification

As stated above, we want to verify block `B` validity against GState
`S`.  To do so, each transaction from block's transaction payload is
taken separately (in order of inclusion into block) and considered
against `S`. If transaction is considered valid, GState is
temporarily [updated](#gstate-modification) and the next transaction
is verified according to new GState. If transaction is considered
invalid, all temporary modifications are discarded. If all
transactions are valid, temporary modifications are committed.

Let's assume that version comparison as described at the end of previos section (Unknown data handling) is done.
Let's denote flag `verifyAllIsKnown` and assign it value:
 * `True` if check is to be done
    - I.e. only known data should be considered valid (unknown data is prohibitied)
 * `False` otherwise

#### General checks

Checks, that are considering whole transaction, not inputs/outputs in particular.

- *Consistency check*: number of inputs in a transaction must be the same as number of witnesses for this transaction
- *Attributes known check*: if `verifyAllIsKnown` is `True`, all transaction attributes must be known.
- *Transaction size check*: size of transaction doesn't exceed size limit
    - Transaction size is computed as number of bytes in serialized `TxAux` (which contains transaction and its witness)
    - Transaction size limit is part of `BlockVersionData`.
    - If the size of transaction is greater than the limit from the adopted `BlockVersionData`, transaction is
invalid.
- *Bootstrap era check*: if the block was created during bootstrap era, all
output addresses must have `BootstrapEraDistr` distribution
  - Predicate "was created during bootstrap era" is checked with
    block's slot field and adopted `BlockVersionData` (information
    about final epoch for bootstrap era is stored in
    `BlockVersionData`).

Also there is a check which applies to whole `TxPayload`:

- Number of transactions must be the same as number of `TxWitness`es.

#### Outputs checks

For each output address we perform the following checks:
1. If `verifyAllIsKnown` is `True`, address' attributes must be known.
2. If `verifyAllIsKnown` is `True`, address' type must be known.
3. Address can't be a redeem address.

#### Inputs checks

For each input `i` we perform:

- *Inputs known check*: if `verifyAllIsKnown` is `True`, type of input
  `i` should have known type
- *UTXO check*: If input `i` is of type `TxInUtxo`, it should have corresponding record in UTXO
- *Difference check*: input `i` must be different from all other
  inputs in transaction. This check is performed only if all inputs
  have known type.

In addition we perform number of consolidated checks (checks which consider whole set of inputs):

* Witness check (separately for each witness)
* Sums check (check inputs sum is greater than or equal to outputs sum)
* Transaction fee check (check transaction fee is fine)

##### Witness checks

Witness check for any type is two-fold:

* check that witness corresponds to the address of corresponding input
* check witness is correct

Witness checks are omitted if (and only if):
* There is an unknown transaction input.

We describe checks for each type in folowing sections.

###### Public key witness

Witness `PkWitness` contains:

* public key `pk`
* signature `sig`

We require:

* Address is a `PubKey` address
* Address spending data is created with public key `pk` (\*)
* Signature `sig` is a valid signature of transaction (validity
  checked with `pk`). Signature inside witness must be given for
  `TxSigData` (which is basically the same as `Hash Tx`) with `SignTx`
  tag. The sequence of bytes to sign is: `01 <magic> HASH_CBOR`, where
  `01` is just one byte, `<magic>` is CBOR-encoded 32-bits constant
  number which differs for mainnet, testnet and other deployments,
  `HASH_CBOR` is the hash of `Tx` we verify.

Note (\*): even though address doesn't contain its spending data, it's easy
to check whether given spending data corresponds to given
address. Suppose we have an address `addr` and spending data
`asd`. Let's say that `addr` has attributes `attrs` and type `t`. We
can construct `Address'` from `attrs`, `t` and `asd` and compare its
hash with the one from `addr`.

###### Script witness

Witness `ScriptWitness` contains:

* validator script, `validator`
* redeemer script, `redeemer`

We require:

* Address is a `Script` address
* Address spending data is created with validator script same as `validator`
* Both `redeemer` and `validator` scripts have the same version.
* If `verifyAllIsKnown` is `True`, script version (which is same for
    redeemer and validator) must be known.
    - For Cardano SL 1.0 it means that version must be equal to 0,
      it's the only version known to our software.
* Plutus validation script built from redeemer and validator for
    given tx must return `True`

###### Redeem witness

Witness `PkWitness` contains:

* redeem public key `pk`
* redeem signature `sig`

We require:


* Address is a `Redeem` address
* Address spending data is created with redeem public key `pk`
* Signature `sig` is a valid signature of transaction (validity
  checked with `pk`) As in public key witness, signature inside redeem
  witness must be given for `TxSigData` (which is basically the same
  as `Hash Tx`). `SignRedeemTx` tag should be. The sequence of bytes
  to sign is: `02 <magic> HASH_CBOR` (it differs
  from [Public key witness](#public-key-witness) in only one byte).


###### Unknown witness

* If `verifyAllIsKnown` is `True`, witness is invalid
* If witness has unknown type with tag `t`, corresponding address must
  have unknown type with tag `t`

##### Sums check

This check is omitted if (and only if):
* There is an unknown transaction input

For each input and output we know its value (i.e. associated balance). We compute sum of all inputs and all outputs.

* If sum of outputs is greater than sum of inputs, tx is considered invalid
* Otherwise, check succeeds, difference between sum of inputs and sum
  of outputs is considered to be *transaction fee*.

##### Transaction fee check

This check is omitted if (and only if) one of conditions met:
* There is an unknown transaction input
* All inputs correspond to `Redeem` addresses
  - Implication of this rule is that one can redeem ADA without paying fee
* Currently adopted tx fee policy is unknown

For this check we know:

* Transaction fee `txFee` (from *Sums check*)
* Transaction size `txSize` (from *Transaction size check*)
* Currently adopted tx fee policy (as part of `BlockVersionData`), which is of known type

###### Size-linear policy
Only policy supported by Cardano SL v1.0 is `TxFeePolicyTxSizeLinear`.

For it, we need to compute minimal required fee for this transaction.
This value is computed using fixed-precision arithmetic (precision is 1e-9) using formula:

```
minFee = a + b * txSize
```

Check succeeds if both conditions hold:

* Minimal fee is non-negative and is smaller than maximal number of coins in the system

   ```minFee >= 0 && minFee < maxBound @Coin```

   - If this fails, it probably indicates there is a mistake in currently adopted fee
   policy

* Actual fee is not less than the minimal fee

   ```txFee >= minFee```

### GState modification

If all checks above pass, we modify GState appropriately. We modify
stakes and UTXO.

#### UTXO modification

UTXO modification is trivial:

* All `TxInUtxo` inputs are deleted from UTXO
* For each output `out` with index `i` we put `(TxInUtxo (hash tx) i,
  out)` into UTXO.

Note, that `TxInUtxo` is only input type supported in CSL v1.0. For unknown inputs GState is not modified.

#### Computing stakes for transaction output

Each `txOut` can be converted to a list of `(StakeholderId, Coin)` pairs.

This is being done with considering `AddrStakeDistribution` attribute of address corresponding to `txOut`:

- For `BootstrapEraDistr` the value `val` of `txOut` is distributed among
bootstrap stakeholders proportional to their weights.

    + If `val >= bootDustThreshold`, then each stakeholder receives
`val * w_ᵢ / weights_sum` coins
      - one stakeholder also receives the remainder (the choice is deterministc)
    + Otherwise:
      - some stakeholders will receive the same stake as their weights
      - one stakeholder may receive less than their weight
      - other stakeholders won't receive anything
- For `SingleKeyDistr id` distribution, the value of `TxOut` will be assigned to stakeholder `id`.
- For `MultiKeyDistr portions` stake will be distributed among multiple
stakeholders according to specified `portions`
     + The first stakeholder may receive slightly more than others due to rounding.

#### Stakes modification

Stakes modification is a bit more complex than UTXO modification, but is also very intuitive:

1. Each `TxInUtxo` input corresponds to some `TxOut`, we compute stakes for it
   * For all inputs, stake lists are concatenated into one, `inStakes`
   * This list denotes how much stake each stakeholder should lose
2. Each output is `TxOut`, we compute stakes for it
   * For all outputs, stake lists are concatenated into one, `outStakes`
   * This list denotes how much stake each stakeholder should gain
3. Stakes of all mentioned stakeholders are updated appropriately
   * Decreased for all `TxInUtxo` inputs
   * Increased for all outputs
4. Total stake is updated

## Local transaction processing

Local transaction processing is needed for transaction relay. Node can
receive a standalone transaction from the network and then it needs to
verify it against its current state (global + mempool) and:

* Apply and relay further (to neighbors)
* Reject

There are few differences between local txp and global txp:

* In local txp we consider not only GState, but also mempool. We
   behave as if transactions in mempool were applied as part of another
   block on top of GState `S`.
   We never modify GState, only mempool is modified.
* We impose a limit on mempool size, specified in a number of
   transactions. If mempool is overwhelmed, we won't accept new
   transactions until we free some space up.
* Transaction verification depends on current epoch (to determine whether to
   apply bootstrap era checks).
   In global txp we know epoch from block header.

   In local txp we use current epoch. If it's
   unknown (which means that our local chain is quite outdated), we
   reject incoming transactions.

* `verifyAllIsKnown` is always set to `True` in local txp (in all related checks).

Note that local transaction processing is basically an implementation
detail and other nodes can do it differently. For example, a node can
always reject all transactions and never relay them.
