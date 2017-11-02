# Transaction processing

## Prerequisites

* You should know what a transaction is, its structure and about tx
  witnesses. You can read about it
  [here](https://cardanodocs.com/cardano/transactions/).
* You also should about technical details about
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
describes how local transaction processing differs from it.

Transaction processing is abbreviated to `txp` in code and some other
places.

## Global transaction processing

Global transaction processing can be described by presenting an
algorithm to solve the following problem: given a sequence of blocks
`B₀, B₁, B₂, …` (where `B₀` is the first genesis block) check whether
transactions from these blocks are valid. The algorithm is similar to
the one from Bitcoin (for example, UTXO is used to prevent
double-spending), but is more complicated because there are more
features.

For verification we maintain some state (called GState) which
corresponds to the application of a sequence of blocks. It's
associated with the hash of the header of the last applied block
(i. e. this hash is integral part of this state). So, given a sequence
of blocks `B₀, B₁, B₂, …` we can compute sequence of states `S₀, S₁,
S₂, …`. There is also genesis state `G` which takes places before any
block is applied. This state is derived from genesis data (see ??? TODO).

In theory transaction verification can be stateless (except genesis
state `G`) and be described without mentioning any additional state
(apart from blocks themselves). But in practice it would be very
inefficient and inconvenient, so we present the verification algorithm
in a stateful way.

The problem: given GState `S` and a block `B` return either an error
describing why `B` is invalid (w.r.t. tx payload) or new GState
`S'`. Genesis blocks don't have tx payload and are ignored by global
txp. Recall that tx payload contains transactions (stored in Merkle
tree) and their witnesses.

### GState

In this section we describe parts of GState relevant to transaction
processing.

* UTXO (unspent transaction outputs). It's a map from `TxIn` to
  `TxOutAux` which contains all unspent outputs. `TxOutAux` is just an
  alias for `TxOut`. Later it can be extended if we want to associate
  more data with unspent outputs (e. g. slot of the block in which
  this transaction appeared).
  For example, if a transaction `A` has 1 output (`out1`) which hasn't been
  spent yet, UTXO will have a pair `(TxIn (hash A) 0, out1)`.
* Stakes. They are not needed for tx verification, but they are needed
  for other parts of the protocol (for example, leader election
  process called FTS) and they are maintained by tx processing.
  We store total stake and a map from `StakeholderId` to its stake (`Coin`).
* Adopted `BlockVersionData`. This data type contains various
  parameters of the algorithm which can be updated by Update
  System (details are not covered in this document). Txp doesn't
  modify this value, it only reads it. Some values are relevant for
  transaction processing:
  * `maxTxSize` – maximal size of a transaction.
  * `scriptVersion` – determines the rules which should be used to
    verify scripts.
  * `txFeePolicy` – determines the rules to check whether a
    transaction has enough fees.
  * `unlockStakeEpoch` – epoch when bootstrap era ends.

### Unknown data checks

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

Let's assume that we have already compared these two versions and know
whether we permit unknown data in blocks. Let's call a flag which
determines whether all data must be known `verifyAllIsKnown`.

Note: when we say that attributes must be known, it means that
unparsed fields must be empty.

### Outputs checks

For each output address we perform the following checks:
1. If `verifyAllIsKnown` is `True`, address' attributes must be known.
2. If `verifyAllIsKnown` is `True`, address' type must be known.
3. Address can't be a redeem address.

### Consistency check

There is only one consistency check: number of inputs in a transaction
must be the same as number of witnesses for this transaction.

### Transaction attributes check

If `verifyAllIsKnown` is `True`, transaction's attributes must be known.

### Inputs-dependent checks

Some checks heavily depend on whether there is at least one input
with unknown type. There are three cases:

1. There is such input and `verifyAllIsKnown` is `True`. In this case
   transaction is immediately considered invalid.
2. There is such input and `verifyAllIsKnown` is `False`. It means
   that there was a protocol update not known to our software. In this
   case we can't know whether that input is valid and how much value
   it has. So we perform only basic
   checks: [outputs checks](#outputs-checks)
   and [consistency check](#consistency-check). Note that we don't
   perform
   [transaction's attributes check](#transaction-attributes-check),
   because `verifyAllIsKnown` is `False`.
3. All inputs have known format (i. e. `TxInUtxo`). In this case
   (which should be the most common one) we can check all inputs'
   legitimacy, compare input and output sums, check fee, etc.
   These checks are described in more details below.

In the following (until [tx size check section](#tx-size-check)) we
consider case (3), when all inputs are `TxInUtxo`. The next step is to
lookup all inputs in UTXO. If at least one input is not found,
transaction is invalid. We further assume that for each input we know
corresponding unspent output. All inputs must exist in UTXO.

All inputs of a transaction must be different and each input must be
properly certified by its witness.

#### Witness checks

Witness checks consist of two parts: we need to check that witness
corresponds to the address from the output we want to spend and we
need to check that witness itself is correct.

* If witness is `PkWitness` (which contains a public key and a
  signature), we require that address is a `PubKey` address and its
  spending data is `PubKey` spending data with the same key as in
  the witness. Witness itself is checked by checking validity of the
  signature stored in this witness. Note: signature inside witness is
  given for `TxSigData` (which is basically the same as `Hash Tx`)
  with `SignTx` tag.
* If witness is `ScriptWitness` (which contains validator and redeemer
  scripts), we require that address is a `Script` address and its
  spending data is `Script` spending data with validator script from
  the witness. Witness itself is valid if few conditions are met.
  * Both redeemer and validator script have the same version.
  * If `verifyAllIsKnown` is `True`, script version (which is same for
    redeemer and validator) must be known. Currently it means that
    version must be equal to 0, it's the only version known to our software.
  * Plutus validation script built from redeemer and validator for
    given tx must return `True`.
* If witness is `RedeemWitness` (which contains a redeem public key
  and a redeem signature), we require that address is a `Redeem`
  address and its spending data is `Redeem` spending data with the
  same key as in the witness. Witness itself is checked in the same
  way as `PkWitness` except that signature scheme is the one from
  RSCoin and signing tag is `SignRedeemTx`.
* If witness has unknown type with tag `t`, corresponding address must
  have unknown type with tag `t`. Witness itself is valid only if
  `verifyAllIsKnown` is `False`.

Note: even though address doesn't contain its spending data, it's easy
to check whether given spending data corresponds to given
address. Suppose we have an address `addr` and spending data
`asd`. Let's say that `addr` has attributes `attrs` and type `t`. We
can construct `Address'` from `attrs`, `t` and `asd` and compare its
hash with the one from `addr`.

#### Sums check

Recall that at this step we assume that all inputs are known and
have been resolved into corresponding unspent outputs of previous
txs. So for each input we know its value. We then compute sum of all
inputs and all outputs. If sum of outputs is greater than sum of
inputs, tx is considered invalid. Otherwise this check succeeds and
the difference between sum of inputs and sum of outputs is considered
to be transaction fee.

#### Fee check

Fee check is performed for all transactions except those where all
inputs correspond to `Redeem` addresses. So it's free to redeem ADA.

By this time we already know how much fee the transaction includes. We
also know transaction's size. Fee check depends on the currently
adopted `TxFeePolicy` (part of `BlockVersionData`). There are two
options:
1. Fee policy is `TxFeePolicyTxSizeLinear`. In this case we need to
   compute minimal required fee for this transaction. This value is
   computed using fixed-precision arithmetic (precision is 1e-9) using
   formula `a + b * txSize`. If minimal fee is negative or is greater
   than maximal number of coins in the system, transaction is rejected
   (it probably indicates there is a mistake in currently adopted fee
   policy). Then we compare actual fee of tx with minimal required fee
   and if it's greater than or equal to the minimal value, transaction
   is valid with regards to this check.
2. Fee policy is `TxFeePolicyUnknown`. In this case the check is
   skipped (i. e. transaction is considered valid with respect to fee
   policy). It's questionable which behavior we want in this case, but
   currently it's implemented this way.

### Tx size check

We compute transaction size as number of bytes in serialized `TxAux`
(which contains transaction and its witness). Transaction size limit
is part of `BlockVersionData`. If the size of transaction is greater
than the limit from the adopted `BlockVersionData`, transaction is
invalid.

### Bootstrap era check

If the block was created during bootstrap era (which is easy to check,
because every main block contains `SlotId` in which it was created and
we know when bootstrap era ends from adopted `BlockVersionData`), all
output addresses must have `BootstrapEraDistr` distribution.

### GState modifications

If all checks above pass, we modify GState appropriately. We modify
stakes and UTXO. UTXO modification is trivial:
* All `TxInUtxo` inputs with are deleted from UTXO.
* For each output `out` with index `i` we put `(TxInUtxo (hash tx) i,
  out)` into UTXO.

Stakes modification is a bit more complex, but is also very
intuitive. We have `TxOut`s corresponding to inputs and outputs of
transaction. Each `TxOut` can be converted to a list of
`(StakeholderId, Coin)` pairs. It depends on `AddrStakeDistribution`
of given address.
* For `BootstrapEraDistr` the value of `TxOut` is distributed among
bootstrap stakeholders proportional to their weights. The behavior
depends on whether the value is less than the sum of weights of all
stakeholders (this sum is called `bootDustThreshold`). If it's greater
than or equal to `bootDustThreshold`, then each stakeholder receives
`val * w_ᵢ / weights_sum` coins (`val` is the value of `TxOut`) and one
stakeholder also receives the remainder (the choice is
deterministc). If it's less than `bootDustThreshold`, then some
stakeholders will receive the same stake as their weights, one
stakeholder may receive less then their weight and other stakeholders
won't receive anything.
* If distribution is `SingleKeyDistr id`, then the value of `TxOut` will
be assigned to `id`.
* In case of `MultiKeyDistr` stake will be distributed among multiple
stakeholders according to specified portions. The first stakeholder
may receive slightly more than others due to rounding.

Then these lists are concatenated and we have two lists of
`(StakeholderId, Coin)` pairs: the first one is how much stake each
stakeholder should lose, the second one is how much stake each
stakeholder should gain. Then stakes of all mentioned stakeholders are
updated appropriately. Also total stake is updated.

## Local transaction processing

Local transaction processing is needed for transaction relay. Node can
receive a standalone transaction from the network and then it needs to
verify it again its current state (global + mempool) and apply or
reject. If it's applied, it's also relayed further, so that other
nodes in network become aware of a valid transaction. There are few
differences between local txp and global txp:

1. In local txp we consider not only GState, but also mempool. We
   behave as if transactions in mempool were applied as part of a
   block. We never modify GState, only in-memory data is modified.
2. We also have a limit on mempool size, specified in a number of
   transactions. If mempool is overwhelmed, we won't accept new
   transactions until we free up some space.
3. Transaction verification depends on epoch (to determine whether to
   apply bootstrap era checks). In global txp we know
   epoch from block header. In local txp we use current epoch. If it's
   unknown (which means that our local chain is quite outdated), we
   reject incoming transactions.
4. `verifyAllIsKnown` is always set to `True` in local txp.

Note that local transaction processing is basically an implementation
detail and other nodes can do it differently. For example, a node can
always reject all transactions and never relay them (which is bad).
