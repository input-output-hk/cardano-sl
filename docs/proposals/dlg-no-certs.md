# No-certificate delegation discussion

In this document we consider posisbility of excluding certificate-driven delegation from CSL's feature.
We discuss various use cases, how they were solved with certificate-driven delegation and how they can be solved without certificates (no-certificate delegation here and after).

## Some technical details and terminology

**Stakeholder id** is an identity of stakeholder on blockchain.

Recall that each ADA coin has two faces: balance and stake.

Balance of coin is associated with some record in Utxo called transaction output. Part of transaction output, an address denotes way to transfer coin (along with other coins) to different owner (designated by different address). 

Stake of coin is associated with *stakeholder id* which is by definition a hash of public component of key which is allowed to participate in protocol on behalf of the coin.

Via transaction (coin transfer) one can reassign both balance and stake of coin.

### Identity of delegate

For both with-certificate and no-certificate delegation, we asume that delegate has associated *stakeholder id* which uniquely designates delegate on blockchain and is known to everyone (along with delegate's market name and etc).

### Solution with certificates

Initially each stakeholder `S`:

 1. Creates a `Ed25519` secret key `S_sk`
 2. Derives identity for himself: `S_id = hash ( public_key ( S_sk ) )`
 3. Associates every coin he owns with this `S_id`


Then when `S` wants to delegate to `D` (with stakeholder id `D_id`), he will:

 4. `S` constructs and publishes delegation certficate:

     * All stake associated with `S_id` should be reassociated to `D_id`
     * This association is valid while delegation certificate remains valid
     * Certificate to be published on blockchain:
         * Either immediately (as with current heavyweight delegation)
         * Or later, as part of created block (as with current lightweight delegation)

### Solution with no certificates

In solution with no certificates `S` associates his coins directly with `D_id`.
I.e. each coin `S` posseses will have stake associated with stakeholder id `D_id`.

## Use case 1. Fees for delegatee

Let's consider a stakeholder `S` who delegated his funds to delegate `D`.

Stakeholder `S` should have sme incentive to delegate his funds to exactly `D`, not to `D1` or `D2`. So it's essential that `D` should have ability to incentivize `S` to prefer it among others.
There are various ways of incentivizing, but perhaps the most important is sending funds to `S`.

This fund sending from `D` to `S` may be designed in different ways:
	W1. Sending `S` a fixed amount of ADA every epoch (fixed amount proportional to stake `S` delegated to `D`), pretty much like bank deposits
	W2. Sending `S` a portion of fees obtained for created blocks (when coin of `S` won the lottery and allowed `D` to create block)
	W3. Various variations like fixed bonus if `S` delegated more than `X` ADA in sum

### Solution with certificates

It's clear how `D` should send fees to `S`:
     * Delegation certificate may contain an address `S_b` (for balance of `S`)
     * `D` sends transactions to `S_b`

### Solution without certificates

In solution with no certificates `S` associates his coins directly with `D_id`.
For that reason, `S` effectively has no `S_id` on blockchain.

Importantly, common practice is that `S` uses different addresses for coin's balance. Rephrasing, if we consider all coins in ownership by `S`, only some of them will share same addresses in their balance association.

So if we assign all `S` coins to stakeholder id `D_id`, for delegate `D` these coins from `S` will accumutively represent set of addresses `B` (by corresponding balance associations of coins). If we also consider there is another stakeholder `S'` which is represented by address set `B'`, there will be no way to distinguish this `B'` from `B`.

For that reason only way to pay fees from `D` to `S` is to send transaction directly to balance addresses from set `B`.

#### Drawback

This reduces flexibility in strategies to send fees, e.g. W3 is not feasible.
W1 and W2 are doable but with significantly more overhead (instead of sending fee transaction to single addresses, you have to send transaction to M addresses corresponding to `D`).



## Use case 2. Hot/cold key management

Suppose you want to launch a node for delegate `D`. Suppose stakeholder key from node will get hijaked, that will give an attacker full control of stake delegated to `D`. And it will be a non-trivial effort to force all people who delegated to `D` to relefelegate to someone else.

So, problem is secret key `D_sk` might be stolen.

### Solution with certificates

Typical solution to this type of problem is to have a special ("hot") key `D_h_sk` deployed to a node, another secret key `D_c_sk` to be kept on some separate storage (presumably secure, with no access to internet). `D_c_sk` is used to generate some kind of certificate of fact that rights are (temporary) reassigned from `D_h_sk` to `D_c_sk`. But this certificate (or rights) to be specified in such way that `D_c_sk` has way to reassign these rights to different `D_c_sk'` at some moment.

Delegation provides a mechanism to utilize this hot/cold key idea to control stake rights. You assign all rights to key `D1`, then create a delegation certificate which delegates from `D1` to `D2`. And only `D2` is deployed on node. Furthermore, with delegation certificates you can implement redelegation, i.e. facility to further delegate from `D2` to another `D3` (which is even more useful in typical stakeholder/delegate setup).

### Solution with no certificates
 
**To be proposed**

## Other use cases

Following use cases are not (yet) described in much detail:

1. Automatically delegate any stake associated with funds that we will receive in the future
2. Change delegation without moving funds
