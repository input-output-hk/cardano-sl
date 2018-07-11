---
title: Design Specification for Delegation in Shelley
subtitle: 'Status: DRAFT'
author: Philipp Kant
documentclass: scrartcl
toc: t
numbersections: true
# This document uses pandoc extensions to markdown. To compile, use
#   pandoc -o delegation_design_spec.pdf delegation_design_spec.md
...

# Purpose
This document describes the requirements and design for a delegation
mechanism to be used in the Shelley release of Cardano.

Delegation will allow holders of Ada to transfer their rights to
participate in the proof of stake (_PoS_) protocol to _stake
pools_. Stake pools are run by _stake pool operators_, and a person
delegating to a stake pool is called _delegator_, _member_, or
_participant_ of a stake pool.

Introducing delegation is important to increase the stability and
performance of the system:

- We cannot expect every holder of Ada to continuously run a node that
  is well-connected to the rest of the network, in order to write a
  block on rare occasions. Some users might lack the expertise to do
  so. Most users will not have enough stake to warrant running their
  own node. Delegation allows all holders of Ada to participate in the
  protocol, regardless of their technical abilities and the amount of
  stake that they hold. Thus we expect less stake to be offline,
  making the system faster and more resilient against an adversary.
- Even if every user were to run a node that was online all the time,
  it would be hard to keep all those nodes well enough in sync to
  avoid forks and still keep a short slot length. Our delegation
  design is aimed at keeping the number of nodes that produce a
  significant amount of blocks reasonably small (about 100 nodes), so
  that effective communication between them is feasible.

This document covers the design of necessary additions to Cardano in
order to support and incentivise delegation.

# Prerequisites

## HD Wallets

We will use a Hierarchical Deterministic wallet (HD wallet) structure,
as described
in
[BIP-32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki).

# Assumptions

# Requirements
The delegation mechanism should meet a number of requirements.  They
can be grouped into functional requirements that the delegation system
should provide, requirements to the security (both of the overall
system and the funds of individual users), and existing features that
should not be impeded when we add delegation to the system.

## Functional Requirements

### Proof of Eligibility

Any slot leader -- and in particular stake pool operators, who are
elected through stake that is delegated to them -- should be able to
prove when they are eligible to produce a block in a given slot.

### Visibility of Delegation on the Blockchain

We expect stake pools to share their rewards with the delegators.  In
order to do this, there must be evidence for the delegation
happening.  Furthermore, we want the sharing of rewards to be
enforced by the protocol, so the evidence must be recorded on the
blockchain.

### Restricting Chain Delegation

We do not want to allow stake to be re-delegated along a chain
arbitrarily.  We will admit some level of indirection, but not more
than necessary to meet the rest of the requirements.

One reason that we do not want arbitrary chain delegation is that it
makes it harder for delegators to figure out who is ultimately
controlling their stake. Another is that unlimited chain delegation
could open up a Denial-of-Service (DoS) attack vector on the system,
where the attacker posts long delegation chains in order to slow down
processes that depend on delegation, such as leader election or
rewards sharing.

We must also have a mechanism to prevent cycles (such as A delegates
to B, and B delegates to A) which would introduce ambiguity to the
question of who manages stake in the end.

### Cheap Re-Delegation

Changing delegation preferences should be as cheap as possible
(while still using appropriate fees to prevent a denial of service
attack on the blockchain).

### Neutral Addresses

We should provide addresses that can hold value, but do not contribute
to the PoS protocol.  Those might be appropriate for use by exchanges,
which will hold large amounts of value, without legally owning it.

## Security Requirements

### Sybil Attack Protection at Stake Pool Level

It is conceivable that an an adversary might try to take over the
network by registering a large number of stake pools, hoping they
accumulate enough stake to mount an attack just by people randomly
delegating to them.

This Sybil attack on the level of stake pools should be made
infeasible, by requiring stake pool operators to allocate a finite
resource to each individual pool they register. In particular, this
resource cannot be the cost of operating a node, since it is possible
to run multiple pools with one node, so that cost would be constant in
the number of pools an adversary is registering.

### Address Nonmalleability

The system should provide protection against the following attack:

Changing Delegation through Address Malleability

:   Suppose that Alice makes a payment to Bob. In preparation, Bob
    transmits an address belonging to his wallet to Alice, and expects
    Alice to pay to that address. If his wallets later on shows that
    his balance is increased by the expected amount, he considers that
    transaction to be successful. An attacker that wants to increase
    their influence on the PoS protocol changes the address that Bob
    sends in such a way that funds in that address are delegated to
    the attacker, but the funds still show up in Bob's wallet.

    The attack is considered successful if the staking rights for the
    transferred money belong to the attacker after the transaction,
    without Alice and Bob noticing the attack.

### Public Spending Keys Should not be Disclosed Prematurely

Delegation of stake should not involve revealing the public spending
key.  The public spending key should only be revealed once the funds
that are controlled by the corresponding private key are actually
transferred to another address.

### Mitigate Key Exposure

A node run by a stake pool will need to have some key that controls
all the delegated stake, in order to sign blocks.  In case of an
incident where the node is compromised, it should be possible for the
stake pool operator to revoke the key, and replace it with a new one.
This should not require any action by the delegators.

### Handle Inactive Stake Pools

We anticipate that a stake pool operator can cease to operate --
whether they lost their keys, lost interest, died, etc.  We want to
minimise the effect of this to the security and liveness of the
system.

### Avoid Hard Transition

When we make the switch from Byron (where all stake is delegated to
the nodes controlled by the Cardano Foundation, Emurgo, and IOHK) to
Shelley (where Ada holders have the freedom to control their stake),
we should avoid a scenario where a significant amount of stake is
suddenly offline.

This could happen if we automatically revoked the automatic delegation
to the core nodes of the Byron network.

### Change Delegation Without Spending Key

Users of a cold wallet, such as a paper wallet or a hardware wallet,
should be able to delegate the stake corresponding to the funds in the
cold wallet without using its spending key.

## Requirements to Preserve Existing Features

### Master Recovery Key

The whole wallet should be recoverable from one single key (without
any additional information, such as the delegation preferences of the
wallet).

The computational complexity of the recovery process should not be
worse than logarithmic in the number of addresses appearing on the
blockchain, and linear in the number of addresses in the wallet.

### Address Recognition

An HD wallet should be able to recognise its addresses in the UTxO, so
that it can report balances and transaction histories to the user.

### Wallet should be Runnable on Independent Devices

Different user interfaces, running on different devices, should be
able to access and control the same wallet, without transferring
state between them.

We will accept some degradation of behaviour when running the wallet
on different devices:

- Both copies might generate the same fresh addresses
- There can be differences in the reported balance while there are
  transactions in flight that only one of the two copies has knowledge
  of.  In particular, when one copy sends a transaction, that
  transaction will only affect the balance reported by the other
  wallet once it is recorded on the blockchain.
- If the wallets use different delegation preferences, funds sent to
  the wallet might end up being delegated to different pools.

### Maintain Privacy

HD Wallets maintain some level of privacy by using multiple
addresses that are not obviously and publicly tied to the same
wallet.  Delegating stake should not necessarily link the addresses
in the wallet of a delegator.

### Short Addresses

Adding delegation to the system should not increase the length of
addresses more than necessary. Ideally, we should use the necessary
changes to the address scheme to come up with an address length that
is even shorter than in Byron.

# User Stories

## Basic Delegation User Stories

TODO: Add User Stories

## User Stories Related to Incentives

### [CDEC-92] Stake Pool Operator Performance Incentives

### [CDEC-91] Optimal stake distribution

TODO: Add User Stories

# Design of Delegation

## Overview of Delegation

Delegation is a separation of the control over the movements of funds
and the rights in the Proof of Stake protocol that are associated with
those funds.  We achieve this separation by introducing another type
of key: while the rights to move funds are tied to a _payment key
pair_ $K^p=(skp, vkp)$, the rights to take part in the PoS are tied to
the _staking key pair_ $K^s=(sks, vks)$.  Here, $skp$ and $sks$ are
the private keys used for signing, and $vkp$ and $vks$ are the public
keys used to verify signatures.

An owner of funds (i.e., the owner of the payment key of those funds)
can _delegate_ their stake to the owner of a particular staking key
$K^s$ by declaring that the stake rights of those funds should be
controlled by $K^s$.  There are different mechanisms for that, aimed
for different use cases.

## Address Structure

Shelley will introduce three different types of addresses: _base
addresses_, _pointer addresses_, and _enterprise addresses_.  Each address
has the form

$$
\mathcal{H}({vkp}) || \beta
$$

where $\mathcal{H}({vkp})$ is a cryptographic hash of the public
spending key, and $||$ denotes string concatenation.  The types of
addresses differ in the _staking object_ $\beta$, which carries the
staking information.

In addition to those new addresses, the system will continue to
support _bootstrap addresses_ and _script addresses_ as introduced in
Byron.

### Base Address

A base address sets the staking rights directly to a staking key
$(sks, vks)$, and sets $\beta = \mathcal{H}(vks)$.  The staking rights
associated with funds held in this address are controlled by the owner
of $sks$.

### Pointer Address

A pointer address indirectly specifies the staking key that should
control the stake of the address.  It does so by referencing a
certificate that has been published to the blockchain.

Concretely, for a pointer address, $\beta$ is a _certificate pointer_,
given by the tuple $(N_\text{block}, N_\text{tx}, N_\text{cert})$,
where $N_\text{block}$ is the number of a blockin the chain, and
$N_\text{tx}$ is the number of a transaction within that block. This
transaction should, as its $N_\text{cert}$s metadata, contain a
stakepool registration certificate[^pointer-heavyweight] (see
\ref{certificates-on-the-blockchain} below).

[^pointer-heavyweight]: The research paper also allows pointer
addresses to heavyweight certificates, but this is only needed for
offline user wallets with cold staking and enhanced security. This is
also the same case that requires a more relaxed version of chain
delegation, which we decided to drop. So we can also restrict pointer
addresses to point to registration certificates only.

### Enterprise Address

Enterprise addresses allow completely opting out of participation in
the proof of stake protocol.  This might be appropriate for exchanges,
which control, but not own, large amounts of Ada.

For enterprise addresses, $\beta$ is set to a fixed constant value,
making them easily distinguishable from other types of addresses.

When determining the stake distribution for the follow the Satoshi
algorithm, enterprise addresses are completely ignored.  Thus, holding
funds in an enterprise address will not increase the chances for being
elected as slot leader.  Note however, that this effectively decreases
the total amount of stake, which plays into the hands of the
adversary.

### Bootstrap Address

Bootstrap addresses were introduced in Byron, and they always delegate
their stake to a fixed set of seven staking keys, corresponding to the
seven core nodes operated by Cardano Foundation, Emurgo, and
IOHK[^Bootstrap].

[^Bootstrap]: TODO: insert detailed description of bootstrap addresses.

Bootstrap addresses will continue to exist in Shelley, but their use
will be disincentivised.

### Script Address

Another type of addresses present since Byron are script addresses.
For those, it is hard to determine whom the funds actually belong to.
The stake corresponding to funds in script addresses will be excluded
from participation in the proof of stake protocol, just as is done for
enterprise addresses.

### HD Wallet Structure in Shelley

The wallet will be a hierarchical deterministic wallet, according to
[BIP-32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki).

Furthermore, we will require that the tree of addresses has a fixed
depth, and that the wallet will only generate a certain number of new
addresses before old ones have been used. These requirements allow us
to keep wallet restoration from seed cheaper than linear in the total
number of addresses in the blockchain. For details, see
\ref{wallet-recovery-process}.

## Certificates

Certificates allow transferring stake rights to specific staking keys.
They can either be posted to the blockchain as transaction metadata
(stakepool registration or retirement, as well as heavyweight
delegation certificates), or revealed upon use (lightweight
certificates).

### Certificates on the Blockchain

Certificates can be publicly announced to all participants by posting
them to the blockchain, as transaction metadata. They will remain
valid until explicitly overwritten or revoked, as an automatic expiry
would likely increase the amount of undelegated, offline stake. The
following certificates can be posted to the blockchain:

Stakepool Registration Certificates

:   A person planning to operate a stake pool can anounce this by
    posting a _stakepool registration certificate_ to the blockchain.

    The certificate must contain the following information:

    - the public staking key, $vks_\text{delegate}$
    - the parameters that specify the reward sharing function of the
    stake pool (cost and margin of the pool)[^incentives]
	- the minimal amount of Ada that the stake pool operator promises
    to deposit to the stake pool
	- an address to which the rewards for the stake pool operator will
      be sent[^stakepool-piggyback]
    - optionally, a stake pool can include an address to which the
      rewards of the pool that exceed the costs and margin are sent.

      If they do, the stake pool members will not get rewards for
      delegating, and their share will go to the specified address
      instead. This will allow stakeholders who do not want to get
      rewards (possibly for regulatory or tax reasons) to delegate to a
      stake pool that benefits a charity.

    Additional, personal, information on the stake pool operator will
    be hosted separately from the blockchain, see \ref{stakepool-registration}.

[^incentives]: This will be elaborated once this document is merged
with the incentives design document.

[^stakepool-piggyback]: A priori, the rewards for the stake pool could
also be distributed amongst the addresses that use the stake pool's
staking key. However, this would allow anybody to piggyback on the
stake pool by using addresses that use the stake pool's staking
key.

Stakepool Retirement Certificate

:   If a stakepool can foresee that it will cease operations, it can
    announce this intent by posting a _stakepool retirement
    certificate_. It contains

    - the public staking key $vks_\text{pool}$ of the pool
    - the epoch number, starting from which the stakepool will cease
      to operate

    It must be signed by the staking key $pks_\text{pool}$ of the pool.

    After the retirement epoch, any stake that is delegated to this
    stake pool will be disregarded for the PoS protocol. It will not
    take part in the leader election process (similarly to how stake
    in an enterprise address is not considered during the election
    process).

    Stakeholders who delegated to this pool should be notified and
    asked to redelegate by their wallet the next time they are online.

Heavyweight Delegation Certificates

:   Users can transfer the rights of participation in the PoS protocol
    from one staking key to another, by posting a _heavyweight
    delegation certificate_ to the blockchain.  A heavyweight
    delegation certificate is a tuple containing

    - the public staking key delegating its staking rights,
    $vks_\text{source}$
    - the public staking key to which stake is delegated,
    $vks_\text{delegate}$[^heavyweight-pointer]

    It must be signed by $pks_\text{source}$.

[^heavyweight-pointer]: It might make sense to use a certificate
pointer here instead?

Delegation Revocation Certificate

:   Users might want to take control of stake that they had previously
    delegated.  They can do that by posting a _delegation revocation
    certificate_, containing the key for which they want to invalidate
    previously posted delegation certificates. It must be signed by
    the corresponding secret key.

### Lightweight Delegation Certificates

In addition to certificates posted on the blockchain, the system will
also support _lightweight delegation certificates_.  They specify
that the staking rights are transferred from a source key
$vks_\text{source}$ to a delegate key $vks_\text{delegate}$.  In
contrast to heavyweight certificates, they are not posted to the
blockchain, but instead included in the block header when a block is
signed with $pks_\text{delegate}$ (or in a message of the coin-tossing
algorithm when $pks_\text{source}$ is elected as a member of the
committe for randomness generation).

The purpose of lightweight certificates is to enable stake pool
operators to mitigate key exposure, \ref{mitigate-key-exposure}.  The
setup is as follows:

- The stake pool operator registers their stake pool, using a key
  $vks_\text{cold}$.  This _cold key_ is kept securely and off-line.

- The stake pool operator uses $pks_\text{cold}$ to sign a lightweight
  certificate $C$, transferring the staking rights to a _hot key_
  $vks_\text{hot}$.

- The stake pool operator keeps $pks_\text{hot}$, as well as $C$, on a
  node that is on-line, and can sign blocks.  A block signed with
  $pks_\text{hot}$ will be considered valid, provided that $C$ is
  included in its header.

- Should the node get hacked, and the hot key compromised, the stake
  pool operator will create a new lightweight delegation certificate
  $C'$, delegating the staking rights to a new hot key
  $vks_{\text{hot}'}$.

  In order to render $pks_\text{hot}$ useless, it must be established
  that $C'$ takes precedence over $C$.  For this purpose, the
  lightweight delegation certificate will have an additional integer
  field, and certificates with a larger value for this field will take
  precedence.

### Chain Delegation

Chain delegation is the notion of having multiple certificates chained
together, so that the source key of one certificate is the delegate
key of the previous one.

We will only allow a very simple form of chain delegation, where we
have zero or one of each of the following certificates, in that order:

1. heavyweight certificate
2. stake pool registration certificate
3. lightweight certificate

This restricted pattern of chain delegation allows us to satisfy all
requirements, but avoids problematic cycles in the graph of delegation
certificates, and makes it easier for the nodes to track the
delegation patterns.

### Certificate Precedence and Validity

The following rules determine precedence and validity of
certificates.  In particular, they describe what happens when multiple
certificates are issued for a given staking key.

The ordering of blocks and transactions induces a canonical ordering
amongst certificates. Thus, the terms older/newer certificate are well
defined and are used below.

#### Stake Pool Registration and Retirement Certificates

- There can be at most one active stake pool registration certificate
    for any given staking key. A newer certificate will override an
    older one.

    This will allow stake pool operators to update their costs and
    margin if they need to. Stake pool members should be notified of
    such changes by their wallet the next time they are online.

- A revocation certificate is only valid if there is an older
  registration certificate.

#### Heavyweight Delegation and Revocation Certificates

- Newer heavyweight certificates override older heavyweight
  certificates. This allows delegators to move from one stake pool to
  another.

- Revocation certificates revoke the effect of older (but not newer)
  heavyweight certificates. So users can join a staking pool, leave
  it and control their stake directly, and still have the opportunity
  to join a staking pool at a later point in time.

#### Lightweight Delegation Certificates

For lightweight certificates, we cannot rely on the ordering induced
by the blockchain. But we do have the counter field, which serves the
purpose of establishing precedence:

- A lightweight certificate with a higher counter overrides one with a
  lower counter.

### Additional Local Node State

It is not sufficient for certificates to be posted to the blockchain:
since nodes will need to validate signatures on new blocks in a timely
manner, they need ready access on all valid certificates without
resorting to the blockchain itself. Distributing rewards
(\ref{sharing-rewards}) requires further additional state.

Nodes will have to maintain the following local databases as they
process blocks:

#### Stake Pools

Pointer addresses (\ref{pointer-address}) need to reference a specific
stake pool registration certificate. Since this is part of the
address, the key should be short. A canonical unique index that is
reasonably short is the certificate pointer described in
\ref{pointer-address}.

Access patterns:

- Lookup by certificate index whenever the staking rights for a
  pointer address have to be resolved
- Lookup by public staking key (to retrieve reward sharing policy for
  a given pool)
- Bulk listing to display active stake pools to the user

#### Active Heavyweight Certificates

All valid heavyweight certificates need to be kept in a local
database.

Access patterns:

- Lookup by source staking key for leader election and rewards sharing

In order to determine which certificate was valid during a given
epoch, we will have an additional field that specifies when the
certificate came into effect, via their certificate index. Old
certificates (revoked or overriden) can be dropped from the database
once the rewards for their last active epoch have been distributed.

#### Addresses and Associated Balances per Staking Key

At two points in the protocol, we will need to know which addresses
belong to a specific staking key, and what their balances are: leader
election for the upcoming epoch, and sharing out rewards for a past
epoch.

The Follow the Satoshi algorithm for leader election needs a list of
staking keys and their associated balances.

For rewards sharing, we need, for each staking pool, a list of all the
pointer addresses with their balances that delegated directly to the
stake pool. We will also need to have the amount of stake that each
heavyweight certificate contributed to the pool.

To achieve both, nodes will maintain a database that contains, for
every staking key, the addresses that are directly -- i.e., ignoring
heavyweight certificates -- controlled by it (both pointer and base
addresses), as well as their balances. Together with the active
heavyweight certificates, this gives us everything we need for leader
election and rewards sharing.

Note that directly tracking the stake for each key, including
heavyweight certificates, would be problematic in case a heavyweight
certificate is overridden or revoked.

#### Updating Local State

**TODO: Update this section**

Each node (that has to verify blocks) will maintain a database with
valid certificates. In the following, we will describe how that
database is modified when adding or rolling back blocks.

Note that there is a general design decision we should consider
regarding rollbacks. The current status is that a node will record
information on how to undo a block when it adds it to its chain. The
new wallet, however, follows a much simpler approach and just keeps
snapshots of the last $k$ slots[^snapshots]. Rolling back is then as
simple as setting the state to one of those snapshots. The following
discussion assumes that we save undo information, as is currently done
in the core. Changing it to snapshots would simplify the design.

[^snapshots]: **TODO**: or is it blocks?

Adding a new block

: Any heavyweight certificate that is valid is added to the local
    certificate database.  Those certificates are also added to the
    undo information, to be deleted when the block is rolled back.

    For each certificate in the new block, we check if there is an
    older certificate for the same source key.  Those certificates are
    now invalid.  They are removed from the certificate database, and
    added to the undo information, so that they can be added again
    should the block be rolled back.

    Any stakepool registration certificate that is found is added,
    unless there is already a stakepool registration certificate for
    the same $vks$ on the blockchain.  Overwriting stakepool
    registration certificates is not allowed, since that would allow
    stake pool operators to change their reward sharing policy without
    consent from their participants.  The certificate is also included
    in the undo information, so that it can be removed in case the
    block is rolled back.

    **TODO**: what about removing stake pool registration certificates
      to retire pools?

    Note that there is a natural order on the certificates in a block,
    induced by the order in which transactions appear in the block,
    and certificates are processed in this order.  This might be
    relevant for resolvig conflicts in case there are conflicting
    certificates within a block.

Rolling back a block

:   Certificates that are in the undo information for deletion are
    deleted from the local certificate database.

    Certificates in the undo information to be added are added to the
    local certificate database.

The certificate database will require resources on each node, so there
should be incentives for actions that keep it small, such as removing
certificates that are no longer needed.

## Delegation Scenarios

### Stakepool Registration

Publicly announcing a stake pool for other people to delegate to
requires two steps: posting a stakepool registration certificate to
the blockchain, and providing additional verifiable personal information.

The second step is essential to establish trust in a stake pool.
However, storing personal information directly on the blockchain would
lead to violation of legislation like the GDPR, so instead of
including it in the certificate, it will be stored on an external
key-value store, using $\mathcal{H}(vks)$ as key.  The integrity of the
data can be ensured by requiring it to be signed with $pks$.

A stake pool operator can change its costs and margin by replacing the
registration certificate of the pool with a new one. This allows
operators to react, for example, to a change in its costs or the
exchange rate of Ada. A wallet that is delegating funds to this stake
pool should notify the user of such a change whenever it detects it,
and ask whether the delegation should be reconsidered.

The rewards that a stake pool gets depend on a deposit of funds that
the stake pool operator themself provides. This adds a cost to
creating a competitive stake pool, and protects against Sybil attacks
on the stake pool level
(\ref{sybil-attack-protection-at-stake-pool-level}). All funds in base
addresses with $vks$ as the staking key are considered to belong to
this deposit.

A stake pool operator will pledge to deposit a certain amount of Ada
to the pool when registering a pool. This pledge is important:
otherwise, an adversarial stake pool operator could circumvent the
Sybil protection of the deposit, by placing a deposit in a pool until
it attracted stake, and then simply moving the stake to the next
pool. The pledge will be enforced at the point of leader election;
stake pools that have a deposit less than what they pledged will be
excluded from the election, and as a consequence forfeit their rewards
for that epoch[^allow-underfunded].

[^allow-underfunded]: We could also just deny the rewards for this
pool, but still let it take part in the protocol.

Note that it will still be possible for a stake pool operator to
decrease the amount of stake that they pledge to the pool, but this
will require them to post a new certificate, which will notify the
stakeholders that delegated to the pool, possibly triggering a
re-delegation.

### Display of Stake Pools in the Wallet

The wallet software will keep a list of all the stakepool registration
certificates it finds.  For each, it will perform a lookup of the
contained $pks$ to retrieve the corresponding metadata to display to
the user.

In order to prevent relying on a central party to host this key value
store, it will be possible to register multiple servers in the wallet,
and each of those will be queried.  Anybody will have the opportunity
to run a stakepool registration server, and announce its existence off
band.

In order for stakeholders to be able to delegate their stake to a
pool, the wallet will provide a listing of stake pools. This listing
will be ordered by the rewards that a user should expect if they were
to delegate to that pool. Since those expected rewards depend not only
on the costs and margin set by the stake pool operator, but also on
the performance of the pool and on the amount of stake that it has
accumulated, this will prefer pools that are reliable, and have not
yet reached saturation. In other words, the users selfish interest to pick a
stake pool that is promising large rewards is aligned with the goal of
placing the system in the hands of a number of reliable stake pool
operators, and of avoiding centralisation. The influence of the stake
pool operator's deposit on the rewards provides protection against a
Sybil attack on the stake pool level
(\ref{sybil-attack-protection-at-stake-pool-level}).

For estimating the rewards shared by a pool, the wallet needs to
predict the performance of the pool, i.e. the ratio of blocks that the
pool added to the chain and the number of slots it was elected as
leader. This is done by assuming the performance to be consant, and
using the performance during the last epoch, which is visible from the
blockchain.

In order to prevent a slight difference in the expected returns to
result in people conglomerating to a single stake pool, filling it
rapidly, the order of the list of stake pools will be jittered: for
each stake pool, the wallet will draw a random number $r$ close to
$1$, and multiply the expected returns by $r$. I propose to draw $r$
uniformly from the interval $[0.95, 1.05]$, but this choice is
arbitrary and should be re-evaluated during the operation of the
testnet.

Since the actual amount of stake that the leader themself uses for the
pool might change at any point in time, the ordering of pools will use
the amount of stake that the leader _pledged_ when registering the
pool, not the amount of stake that the leader currently put into the
pool. However, pools where the current deposit is smaller than the
amount pledged are expected to give zero rewards, and will end up at
the end of the list.

### Basic Delegation

When a user has chosen a stake pool $P$ to delegate to, new addresses
that the wallet generates will be pointer addresses
(\ref{pointer-address}) pointing to the registration certificate of
$P$. This will cause all the funds that the wallet will receive to
those addresses to be delegated to $P$.

Additionally, the wallet will provide the option to automatically
re-delegate all funds currently in the wallet to $P$. If this option
is chosen, the wallet will create a new address (pointing to $P$), and
transfer the funds it controls to this new address. Note that a single
transaction can have multiple inputs, so this will not require a large
number of transactions, and incur only moderate costs, as required by
\ref{cheap-re-delegation}.

Using delegation via pointer addresses does not obviously link
addresses of the same wallet, as required by \ref{maintain-privacy},
though it does group addresses that delegate to the same
pool. Choosing the option of automatically re-delegating _does_ link
addresses, by using bulk transaction, but that option is not
required.

### Delegation of Cold Wallets

Using pointer addresses for delegation requires to move funds to a new
address in order to re-delegate. For hot wallets, this is fine, but
not so for cold wallets: cold wallets are meant to be placed in a
vault or buried underground for long-term safe storage, while the
owner might still want to re-delegate the funds therein from time to
time.

In order to facilitate re-delegation of funds stored in a cold wallet,
cold wallets will use base addresses (\ref{base-address}) with one
common staking key $(sks, vks)$. In order to (re-) delegate, the
owner of the wallet will use a hot wallet to issue a transaction
containing a delegation certificate using $sks$. The second wallet only
needs to contain a small amount of funds to pay for the necessary
transaction fees, so the requirement of it being a hot wallet is not a
significant security risk.

### Individual Staking

Stakeholders are not required to delegate their stake to a pool. If
they wish to run their own node, they should use base addresses with a
common staking key, and use that key to sign blocks with their
node.

In addition, they _can_ post a stake pool registration certificate,
with a margin of $m=1$ (for which they are not required to upload any
personal information). Usually, this should not be necessary. However,
without the registration certificate, it is possible for a third party
to piggy-back on such a private node, by using addresses in their
wallet that use the same staking key. The rewards distribution
mechanism will not be able to discern which addresses truly belong to
the stakeholder operating the node, so the third party will get some
rewards for this not-asked-for delegation.

Posting a "private" registration certificate with $m=1$ will ensure
that all rewards are sent to the address specified in the certificate.

### Rewards

For the smooth operation of the system, it is beneficial to have a
large portion of the stake delegated to a set of reliable stake
pools.  Thus, we should incentivise delegating stake to reliable stake
pools.  One way to do this is to have stake pools share their rewards
with their participants.

The reward sharing mechanism should satisfy the following
requirements:

1. Sharing rewards should be an automatic process that does not
   require an action, neither by the stake pool operator nor the
   participants. This requirement is not only meant to ensure that
   participants get their share reliably. The share of the rewards
   that are given to a particular participant depends on the amount of
   stake that that participant delegated in a particular epoch. Thus,
   any node that verifies a transaction that transfers the rewards for
   a given epoch needs to access the staking information for that
   epoch. While this information is archived on the blockchain
   indefinitely, looking it up for arbitrary past epochs might be too
   costly. Making the sharing of rewards an automatic process in the
   following epoch circumvents this problem.

2. Sharing rewards should not lead to an excessive growth of the
   UTxO. In particular, it should avoid creating dust entries.

3. Sharing rewards should not lead to a burst of transactions that
   risks pushing the system to the limits of its predictable region of
   operation.

4. Sharing rewards should not increase the linkability of addresses of
   a wallet.

5. The reward sharing policy of the stake pool should be transparent
   to potential participants.

In order to satisfy requirements 1-4 above, the rewards for stake pool
members will not be dealt out via transactions. Instead, the UTxO set
will be adjusted automatically when the system goes from one epoch to
the next. Requirement 5 is satisfied, since the rewards for stake pool
members are determined by the cost and margin parameters that the
stake pool operator includes in the stake pool registration
certificate (as well as by the performance of the stake pool).

#### Distributing Rewards

At the end of an epoch, stake pool operators and members will be
rewarded, via an update of the UTxO set and a special
transaction. Since all the information needed to compute the rewards
is publicly available on the blockchain, this update can be calculated
and checked by each node independently.

Upon reaching the end of an epoch, each node will calculate the update
to the UTxO that distributes the rewards for the preceding epoch, in
the following manner[^merge-rewards]:

[^merge-rewards]: This section contains references to the rewards and
incentives design document, which will be merged with this document
soon.

Let $t_0$ be the point in time at which the leader election for the
epoch took place. For each staking key that had been selected as slot
leader for at least one slot during the epoch, calculate the pool
rewards $\hat{f}$, based on the performance during the epoch,
and the stake deposit and delegated stake at $t_0$. Note that the
stake that the leader contributed to the pool can be differentiated
from the stake that pool members delegated: the former uses base
addresses with the pool's staking key, the latter either pointer
addresses or delegation certificates.

Pool Member Rewards

: If the staking key belongs to a stake pool that did not specify a
    special address to which all member rewards should be sent, the
    pool member rewards will be calculated _per UTxO entry that
    delegated to the pool_. This is necessary, since we do not wish to
    link together the addresses of individual stake holders, and
    sharing rewards _per pool member_ would require us to do just
    that. It is possible, since the rewards for each member are linear
    in the stake that that member delegated (once $\hat{f}$ is fixed).

    The value of each UTxO entry that did delegate at $t_0$ will be
    updated, adding the reward $r_\text{member}$ calculated for the
    stake of this entry.

    Note that some transaction outputs which were unspent at $t_0$
    will have been spent during the epoch. Those will not get
    rewards. The rewards they would have gotten (as well as any
    fraction of a reward smaller than a Lovelace that got rounded
    down) will be treated as all rewards that are not distributed:
    they are partly assigned to the treasury and partly carried over
    to the next epoch.

    This disincentivises stakeholders from re-delegating, and from
    using or moving their funds. If we want to avoid this, an
    alternative is described in section
    \ref{updating-at-the-start-of-an-epoch}.


Pool Leader Rewards

: The rewards for pool leaders can not be distributed via an update of
    the UTxO, but must be transferred to the address that the operator
    specified when registering the pool. Instead, pool leader rewards
    will be shared out in a bulk transaction.

    This transaction will have an empty input set, and does not need
    to be signed. Its validity can be checked by every node, since it
    can be derived deterministically from the blockchain.

Charity Pools

: The member rewards for charity pools can also not be handled by the
    UTxO update, and are instead included in the transaction that
    distributes the pool leader rewards.

Individual Stakeholders

: If an individual stakeholder posted a stake pool registration
    certificate (with margin $1$), they will be handled just like a stake
    pool by the rewards sharing mechnism.

    Otherwise, the system will assume that all the base addresses
    using the same staking key belong to the same stakeholder. It will
    calculate the rewards for the pool operator, and distribute it
    across the UTxO entries using those addresses, weighted by the
    coins those entries hold.

Note that the size of special transaction for the rewards of stake
pool operators will be rather large, but bounded: it can never have
more outputs than twice the number of slots in an epoch. This
conservative bound will only be reached if every slot, a different
leader was elected, and all of those leaders were "charity" pools
(which is where the factor of two comes from). In practice, the number
of outputs will be much closer to the number of relevant stake pools.

#### Updating at the Start of an Epoch

Cashing out the pool member rewards as depicted above has the drawback
that stakeholders are discouraged from re-delegating, since they would
lose their rewards for an epoch. They are also discouraged from using
their funds for payments.

This can be avoided if the rewards are not distributed after the
epoch, but at $t_0$, the time when the leader schedule is
determined. At this moment, all the UTxO entries that delegated at
$t_0$ trivially exist, and no rewards will be forfeited.

An obvious problem with this is that there is no way to know in
advance how well a given stake pool will perform in the next
epoch. Assuming that the performance of a pool is more or less
constant, we can however use the performance during the previous epoch
(using some fallback, such as the average performance of the whole
system for stake pools that were not elected in the previous epoch).

#### Handling of Bootstrap Addresses

Funds in bootstrap addresses will continue to be delegated to the core
nodes of the Byron network. As long as there are funds in bootstrap
addresses, the core nodes will continue to be elected and produce
blocks. However, there will be rewards neither for the operators of the
core nodes, nor for stakeholders delegating to them. Thus, the core
nodes will contribute to the performance and security of the system,
but their use will be disincentivised. This aligns the individual
user's short-term interest of receiving rewards with the overall goal
of reaching decentralisation of the system.

## Stale Stake

Over time, we expect that an increasing amount of stake will become
inactive. Individual stakeholders might lose their keys or interest in
the system, and stake pool operators might stop operating in an
unorderly fashion without posting a retirement certificate. This poses
two problems for the system: the chain growth will decrease, limiting
the rate at which transactions can be processed, and increasing the
latency. It will also play in the hands of an adversary, since stake
which is offline is counted as adversarial.

Luckily, this stale stake can be detected by looking at the
blockchain: every time a stale staking key is elected, we will get an
empty slot, and a block will be missing in the chain. Of course, a
single empty slot does not need to indicate that the elected staking
key is indeed stale (there might be network issues, a node might have
crashed or been rebooted). But a key that misses multiple slots where
it was elected is likely to be inactive.

The system will consider stake keys that satisfy the following two
conditions to be inactive:

- The key has failed to sign blocks for the last 10 slots where it was
elected as a slot leader[^ten-slots].
- It has not been used to sign a single block during the previous
epoch

[^ten-slots]: The number 10 here is arbitrary and subject to discussion.

The second criterion is meant to prevent large stake pools or
stakeholders from being considered inactive if they experience a
temporary outage outage that is shorter than an epoch, but long enough
to cover 10 slots for which they were elected.

Inactive stake keys will not be considered during leader
election. This ensures that the chain growth is not slowed down by the
inactive stake. It also somewhat improves the security problem:
instead of the inactive stake becoming adversarial, the overall amount
of stake is effectively reduced.

Stakeholders who have delegated to a pool that is considered inactive
should be notified by their wallet the next time they come online, and
the wallet should advise them to re-delegate.

Owners of a stale staking key -- both individual stakeholders and
operators of a stake pool -- should also be notified when their
staking key becomes stale. If they still have the key after it became
stale (for isntance, if their node went down temporarily), they should
have the possibility to announce to the blockchain that their key
should be considered to be active again. They can do that by posting a
_heartbeat_ message, a message that contains the current slot number,
and is signed with the stale key, as transaction metadata. The system
shall recognise such messages, and consider the key to be active
again if it became stale before the slot number mentioned in the
heartbeat. Including the slot number in the heartbeat prevents a
malicious third party from re-using a previous heartbeat message.

## Address Recognition

Wallets will recognise addresses that belong to them just as they
would without delegation, by looking only at the $\mathcal{H}({vkp})$
part of the address.

After a wallet recognises an address for which it controls the payment
key, it will check whether the staking object $\beta$ is set according
to the current delegation preference of the wallet. If there is a
discrepancy, it will alert the user, and ask them whether they want to
re-delegate according to the current delegation preferences.

This check protects against the malleability attack in
\ref{address-nonmalleability}.

## Leader election, Block Validity, and Randomness Generation

The process of leader election has to be modified to take delegation
into account.

When the schedule for the next epoch has to be constructed, the nodes
will compute the stake per staking key, taking into account all
pointer addresses and valid heavyweight delegation certificates. The
result is passed to the Follow the Satoshi (_FtS_) algorithm to choose
a leader for each slot in the next epoch[^sorted-randoms].

[^sorted-randoms]: This can be done traversing the stake distribution
only once, if we generate a list of _sorted_ random numbers, traverse
it in lockstep with the stake distribution, and then shuffle the
resulting list of leaders).

Most stake pool leaders will use lightweight certificates in order to
protect the key to which their members delegated. A block for a slot
where the key $vks_\text{leader}$ has been elected as leader will be
considered valid by all nodes if either

- The block is signed by $vks_\text{leader}$
- The block is signed by $vks_\text{hot}$ and contains, in its
  header, a lightweight certificate that transfers the staking rights
  from $vks_\text{leader}$ to $vks_\text{hot}$

In case there are more than one block for the current slot, each of
which are signed using a lightweight certificate, the newest
certificate (as per the included counter) takes precedence.

The committee for the randomness generation will be chosen in the same
way as the slot leaders, by running FtS algorithm on the stake
distribution.

## Related Topics

### Wallet Recovery Process

Wallet recovery is the process of reconstructing a wallet from the
root key.  In order to reconstruct a wallet, all addresses belonging
to that wallet which appear on the blockchain need to be identified.

In the current implementation, this is done by traversing the
blockchain, and for each address, checking whether it belongs to the
wallet.  Unfortunately, this is linear in the size of the blockchain,
leading to a very poor user experience.

To speed this up, we will reverse the strategy.  Instead of going
through the addresses on the blockchain, checking for each whether it
belongs to the wallet, we go through the possible addresses of the
wallet, and search whether they appeared on the blockchain.

In order for this to be efficient, we need to maintain an index, where
we can look up addresses in the blockchain by some key, and we need to
have a way of generating the key for an arbitrary range of addresses
in the wallet, using only the root key as input.

Recall from \ref{address-structure} that the addresses have the form
$\mathcal{H}({vkp}) || \beta$, where $vkp$ is the spending key, and
$\beta$ depends on the delegation for that address. The
$\mathcal{H}({vkp})$ part is derivable from the root key (in
particular, it does not depend on the delegation preferences of the
wallet), and is a suitable key for the lookup of
addresses[^prefix-search].

[^prefix-search]: Depending on the serialisation format for addresses,
    it might be possible to not use a separate index at all: if
    $\mathcal{H}({vkp})$ is a prefix of the serialised address, we can
    directly do a prefix query in the database.

Of course, we cannot search for _all_ possible addresses of the wallet.
Instead, we utilise the tree structure of the HD wallet.  We will
require that the wallet software populates this tree in a specified way
that will allow us to do a kind of exponential search for the
addresses of the wallet.


#### Trees of Depth 1

To simplify, let us consider a wallet where the HD wallet tree is of
depth 1, so that each address has an index $i \in \mathbb{N}$.  We
will require that the wallet creates addresses in order, and that
there is a _maximal address gap_ $\bar{i}$, such that the address
$\alpha_i$ will not be generated unless there is an address
$\alpha_{i'}$, with $\exists i' \in [i-\bar{i}-1, i-1]$ already
appearing on the blockchain.

The first step in restoring a wallet is to find an upper bound on the
number of addresses of the wallet, $i_{\text{up}}$.  This can be done
by consecutively looking at the intervals

$$
I_{n} = [2^n + i | i \in [0, \bar{i}]], n \in \mathbb{N}
$$

and checking whether any of the addresses in $\alpha_i$ for $i \in
I_{n}$ appears on the blockchain.  This check is performed by creating
the corresponding spending key, hashing it, and doing a look-up in the
index.  For some $n$, this will fail, and we will have found $\bar{i}$
consecutive indices for which there are no addresses of this wallet on
the blockchain.  Because $\bar{i}$ is the maximal address gap, no
address larger than $2^n$ has been created for the address, and we
have $i_\text{up} = 2^n$.

Afterwards, we can perform a binary search for the maximal address
$i_\text{max}$, in the interval $[2^{n-1}, 2^n]$.  In each step of
this binary search, we will probe for $\bar{i}$ consecutive
addresses, starting from an offset $i$.  If none of them exist, we
know that $i_\text{max} < i$, otherwise $i_\text{max} \geq i$.

Finally, we will create all spending key hashes in the range $[0,
i_\text{max}]$, and look up the corresponding addresses.

Two remarks are in order:

Early Finish and Memoisation

: The above process will perform more lookups than necessary.  The
    binary search can be aborted once the search window gets smaller
    than $\bar{i}$.  In addition, we should consider memoising the
    spending keys and/or lookups.

#### Taller Trees

This scheme can be generalised for trees of larger depth.  The current
wallet in Cardano has a fixed depth of 2.  Each address in this wallet
has an index $(i, j) \in \mathbb{N} \times \mathbb{N}$.  In order to
generalise the above wallet restoration procedure for this wallet, we
will require that there is no gap in the $i$, and a maximal
gap $\bar{j}$ in $j$.

Identifying the maximal value $i_\text{max}$ is straightforward: look
at lists of indices

$$
[(i, j) | j \in I_0]
$$

for increasing values of $i$, until there is no address found on the
chain for a specific value of $i$.
Once $i_\text{max}$ is found, we can iterate the method for trees of
depth 1 over all $i \in [0, i_\text{max}]$.

Further generalisations to arbitrary depths are straightforward,
provided that

- all the leaves are at the same depth
- at each depth, we can require a certain maximal gap

### Transition from Bootstrap Phase

As of the time this document is written, Cardano is in the "bootstrap
pahse", where the network is not decentralised, but federated.  All
stake is automatically delegated to seven stakeholders, by requiring
that all the outputs of transactions are to bootstrap addresses (see
\ref{bootstrap-address})[^TODO]. Those stakeholders have posted one
heavyweight delegation certificate to the blockchain, giving control
to seven nodes (the _core nodes_) controlled by Cardano Foundation,
Emurgo, and IOHK.  During the bootstrap phase, rewards are not
collected.

[^TODO]: Check with Erik that this is indeed how it currently works.

The transition from bootstrap phase to delegation is performed by
dropping the restriction of using bootstrap addresses as transaction
outputs.  Moving stake rights away from the core nodes to stake pools
or individual users will require user action.  This is a deliberate
choice: if we, for example, transferred all the stake rights to users
at the end of the bootstrap phase, we would risk to have a large
portion of the overall stake become offline, since most users will
neither be online, nor delegate immediately.  This would pose a risk
to both the performance and, worse, the integrity of the system.

The obvious drawback of keeping the stake rights with the core nodes
until the users intervene is that it will lead to a lesser degree of
decentralisation in the transition period between bootstrap phase and
full decentralisation.  We will probably counter this by incentivising
the users to delegate away from the core nodes.  This could be done by
having the core nodes collect none or fewer rewards (and thus sharing
fewer rewards with delegators).

TODO: Needs definitive input from incentives stream regarding how to
incentivise users to delegate away from code nodes.

# Satisfying the Requirements

In the following, we describe how the requirements listed in
\ref{requirements} are satisfied by the design in this document.

\ref{proof-of-eligibility} Proof of Eligibility

: The leader election process takes delegation via pointer addresses
    and heavyweight certificates into account
    (\ref{leader-election-block-validity-and-randomness-generation}),
	so the leader schedule will contain the key of the party that is
    expected to sign the block (either a stake pool operator or an
    individual stakeholder).

    If a lightweight certificate is used, it will be posted to the
    block header, which will also prove eligibility.

\ref{visibility-of-delegation-on-the-blockchain} Visibility of Delegation on the Blockchain

: Delegation via heavyweight certificates and pointer addresses is
    visible on the blockchain. Delegation via lightweight certificates
    should only be used for hot/cold key management. Thus, it is not
    relevant for the rewards sharing process, and does not need to be
    visible on the chain.

\ref{restricting-chain-delegation} Restricting Chain Delegation

: Chain delegation is properly restricted, as described in
    \ref{chain-delegation}.

\ref{cheap-re-delegation} Cheap Re-Delegation

: Re-delegation can be performed cheaply with multi-input
    transactions.

\ref{neutral-addresses} Neutral Addresses

: The design includes enterprise addresses (\ref{enterprise-address}),
    which are disregarded by the PoS protocol.

\ref{sybil-attack-protection-at-stake-pool-level} Sybil Attack Protection at Stake Pool Level

: Stake pool operators are expected to pledge a deposit to their pools
    that has an influence on the rewards that stake pool members will
    receive, and on the position of the stakepool in the listing
    displayed to stakeholders (\ref{stakepool-registration},
    {display-of-stake-pools-in-the-wallet}, **TODO**: add reference to
    rewards function once document is merged with incentives
    doc).

    Since this pledge cannot be shared between multiple pools,
    creating $n$ viable stake pools will require funds linear in $n$.

\ref{address-nonmalleability} Address Nonmalleability

: Protection against the malleability attack is described in
    \ref{address-recognition}.

\ref{public-spending-keys-should-not-be-disclosed-prematurely} Public Spending Keys Should not be Disclosed Prematurely

: The introduction of a dedicated staking key
    (\ref{address-structure}) avoids the need to use the payment key
    for delegation purposes.

\ref{mitigate-key-exposure} Mitigate Key Exposure

: Stake pool operators can and should use lightweight certificates for
    hot/cold key management, as described in
    \ref{lightweight-delegation-certificates}.

\ref{handle-inactive-stake-pools} Handle Inactive Stake Pools

: We have two mechanisms for dealing with inactive stake pools. Stake
    pools can be retired via a retirement certificate
    (\ref{stake-pool-registration-and-retirement-certificates}. If a
    stake pool ceises to operate without being properly retired, its
    key will be detected as a stale staking key as described in
    \ref{stale-stake}.

\ref{avoid-hard-transition} Avoid Hard Transition

: As described in \ref{transition-from-bootstrap-phase}, at the
    transition from Byron to Shelley, all stake will initially stay
    delegated with the core nodes of the Byron network, avoiding a
    hard transition with temporarily undelegated stake.

\ref{change-delegation-without-spending-key} Change Delegation Without Spending Key

: Delegation of cold wallets is described in
    \ref{delegation-of-cold-wallets}, and does not require having the
    spending key of the cold wallet online.

\ref{master-recovery-key} Master Recovery Key

: Wallet recovery is described in \ref{wallet-recovery-process}, and
    does not require any information in addition to the master key.

\ref{address-recognition} Address Recognition

: Wallets will recognise addresses belonging to it by looking at the
    payment key hash part of the address, as described in
    \ref{address-recognition-1}.

\ref{wallet-should-be-runnable-on-independent-devices} Wallet should be Runnable on Independent Devices

: With the caveats listed in that requirement, nothing in this
    document requires wallets running on different devices to share
    state.

\ref{maintain-privacy} Maintain Privacy

: The default delegation mechanism (\ref{basic-delegation}) uses
    pointer addresses, so addresses of the same wallet are not
    connected in an obvious way.

\ref{short-addresses} Short Addresses

: The goal of having reasonably short addresses has guided the design
    of delegation, and we do not see an obvious way of making them
    even shorter, while still satisfying the rest of the requirements.
