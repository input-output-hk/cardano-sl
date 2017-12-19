# Establishing trust in a snapshot

This document describes the problem of establishing trust in a
snapshot and proposes two ways to solve it. It is needed
for [fast synchronization](#README.md).

## The problem

Suppose there is a node which is launched for the first time. It has a
way to download a snapshot necessary to process new blocks, figure out
wallets' balances, make transactions and all other activities which we
want to support. The problem is to check whether this snapshot is
correct, i. e. corresponds to a valid sequence of blocks. There are
several properties we want to optimize:
* How possible is that incorrect snapshot will be considered correct?
  How costly is it?
* How much data needs to be downloaded (as a function of time and
  maybe other parameters)?
* How many local computations should be performed?

## Why the problem is harder for PoS

In PoW system the problem can be solved in the following way:
* Put hash of snapshot into each `n`-th block.
* Download hash from block with depth in range `[n .. 2 · n)`.
* Obtain actual snapshot `S` corresponding to that hash somehow (out scope
  of this document).
* Download all other blocks after `B`.
* Verify these blocks according to snapshot `S`.
* If blocks are valid, we can trust snapshot `S`.

It works fine in PoW system, because creating `n` valid blocks is very
expensive (assuming `n` is large enough).

However, creating blocks in PoS system is very cheap. So one can
create a snapshot according to which they own all stake and then
create a lot of blocks on top of that snapshot very quickly.

## Solution with checkpoints

This solution is quite simple and requires presence of trusted third
parties. It consists of the following steps:
1. Obtain public keys of trusted parties and supply them to client
   software.
2. Obtain a checkpoint which cointains a public key, a signature and
   a hash of a snapshot. It should also contain hash of the header to
   which the snapshot corresponds, unless it's a part of the snapshot
   (format of the snapshot is not specified yet).
3. Specify a rule to establish trust.
4. Check whether the rule is satisfied. If it's satisfied, download
   the actual snapshot and check its hash. This and further procedure
   and not specified in this document, we only want to obtain a trust
   hash of a recent snapshot. If the rule is not satisfied, we can
   fallback to regular download of blocks or try again later or use a
   weaker rule.

There the following questions to be answered:
1. How can one obtain trusted public keys and supply them to software?
2. How to obtain a checkpoint?
3. How to specify and represent a rule to establish trust?

### Obtain trusted keys

There are two ways:
1. Creator of a client software can embed their public key into their
   program.
2. Keys can be downloaded from official websites of trusted parties or
   printed on paper (probably as a QR code) and shared
   physically. Client software should provide an ability to import
   trusted keys as well as delete imported keys.

The first option is convenient because it works out of box. However,
if a trusted key is compromised, it must be possible to delete it and
download other trusted keys, so the second option is very
important. The second option is also useful for users who don't want
to trust a single party, they can import multiple keys then. So it
makes sense to implement both approaches.

### Obtain checkpoints

The easiest way to do it is to embed a checkpoint into software. Apart
from simplicity, it is probably more secure if software is downloaded
via update system. That's because such software must be approved by
major stakeholders, so in particular it implies that all major
stakeholders approve the checkpoint. This advantage doesn't work if
software was downloaded by other means, e. g. from some website, which
is basically the only way to download software for the first time (you
need to have software to use update system). Unfortunately this
approach it works only for recently released software. It's possible
that software was released and downloaded in March, but launched only
in November (user just forgot about it). In this case hardcoded
checkpoint will be quite old and we'll need a newer one.

So we also need a way to download checkpoints from external
world. There are the following possibilities:
1. Embed some URLs into software and try to download checkpoints from
   them. Again, it shouldn't be the only approach, because this list
   may become outdated (URL may become unreachable or irrelevant).
2. Provide a UI to specify URLs from which checkpoints can be
   downloaded. It might be a good idea to make this information
   available along with public keys. So one major stakeholder can put
   their public key and a list of URLs in some format, presumably with
   some meta information, and then all this data can be imported via
   Daedalus.
3. TODO: I can try to come up with something else. Should I do it
   within this issue? AFAIU, we have two more spaces where we need to
   solve the same problem. Specifically, we need to download actual
   snapshots (it should be described within CSL-2062) and we need to
   download packages and binary diffs for update system.

### A rule to establish trust

Recall that a checkpoint consists of a public key, a signature and a
hash of a snapshot. A
client [stores several trusted public keys](#obtain-trusted-keys)
and [can obtain several checkpoints](#obtain-checkpoints). Checkpoints
with public keys not from our trusted list of public keys can be
ignored. Also checkpoints with invalid signatures should be ignored
(signature should be given for snapshot hash and can be verified using
attached public key from checkpoint).
* The simplest rule is to require that at least one valid checkpoint signed
  with a trusted key exists.
* A simple extension is to let user say that there should be at least
  `x` valid checkpoints with the same snapshot hash (note that
  valid checkpoints can have different snapshots if they correspond to
  different state, one older than another one).
* Also we may require to have signatures for all checkpoints from
  certain parties and at least `x` checkpoints from other trusted
  parties.
* Perhaps it should be enough for virtually all users and no more
  further complication is necessary.

### Properties

#### Correctness of the snapshot

Correctness of the final snapshot depends on honesty of trusted
parties. It's hard to estimate it using math.

#### Data to download

A checkpoint contains a public key, a signature and a hash. We can
think that one checkpoint takes about 200 bytes (it depends on
signature scheme and hashing algorithm, we can choose them
arbitrarily). In most cases it's enough to download 3 checkpoints. So
there is negligible amount of data to download to establish trust (but
not to obtain actual snapshot).

#### Local computations

Loca computations are also negligible in general case. We just need to
check a few signatures.

## Solution with tiny blocks

This solution is fully decentralized, no trusted parties are
involved. The idea is to build another chain which will be much
smaller than the actual chain, but will contain enough data to verify
its validity.

### Description

First of all let's introduce a constant `l ≤ k`. It will be used later.

Apart from actual blocks each full node stores another sequence of
blocks. Let's call them _tiny_ blocks. One tiny block corresponds to one
epoch (`e`) and contains the following data:
* List of stakeholders elected to create last `2 · l` slots in epoch
  `e`. Note that in presence of heavyweight delegation it's not the
  same as the list of slot leaders. For instance, if Alice is a leader
  and delegated to Bob, she can't create a block, so Bob should be put
  in this list. Stakeholders are represented by hashes of their public
  keys.
* Signatures of this list issued by creators of blocks from last `2 ·
  l` slots from epoch `e - 1`. If lightweight delegation certificates
  are used, some signatures may be proxy signatures consiting of two
  simple signatures and one public key. `ProxySig = (Signature,
  Signature, PublicKey)`. The first signature is given for the public
  key fromt this tuple by the stakeholder elected to create a
  block. The second signature is given for the list of block creators,
  as usual.
* Public keys corresponding to signatures (i. e. which can be used to
  verify signatures) from the previous bullet point.

We should also put some extra data into the actual blockchain to be
able to compute tiny blocks. We require all blocks `b` in epoch `e`
after slot `10 · k - 2 · l - 1` until the end of the epoch to have a
signature `sig_e`. This signature must be issued by the creator of
`b`. They should sign list of leaders of last `2 · l` slots in epoch
`e + 1` (which can be computed from blocks prior to `b`). If
stakeholder creates more than one block in this interval, only the
first one should contain the signature.

Another extra data which should be put into the actual blockchain is
hash of recent snapshot. For example, in each epoch we can put
snapshot corresponding to the state before `6 · k`-th slot into the
first block after `8 · k`-th slot.

It's not necessary to put signature `sig_e` or snapshot hash into
header, they can be put into block (and their hash should be part of
header for integrity).

#### Maintenance of tiny blocks

Full nodes which have actual blocks can easily maintain tiny blocks. A
tiny block for epoch `e` can be created when `e` starts. At that
moment we know leaders of new epoch and have last `2 · l` blocks from
epoch `e - 1`.  Those blocks contain signatures of list of new leaders
and corresponding public keys. So there is all necessary data to
create tiny block for epoch `e`.

#### Fast synchronization procedure

If node lacks a lot of blocks and wants to quickly obtain a recent
snapshot, it does the following:
1. Send few most recent `HeaderHash`es of last accepted blocks to one
   or more peers, just like it's done for regular synchronization.
2. The peer finds the most recent header among received ones in their
   blockchain. Suppose this header is from epoch `e`. Note that it's
   important to send more than one hash, because node could be stopped
   before fork happened and may have newest blocks which are not part
   of the main chain. So the node has all blocks from all epochs
   before `e`.
3. Then the peer sends a bunch of tiny blocks starting from one for
   epoch `e`.
4. When the node receives a tiny block for epoch `e`, it knows who can
   create blocks in that epoch. So it can verify that signatures from
   the tiny block are legitimate and provided by actual slot leaders.
5. Now the node knows who can create blocks in last `2 · l` slots in
   epoch `e` (this information is available from tiny block for
   epoch `e`). It can then verify signatures from tiny block for
   epoch `e + 1`.
6. Repeat step 5 for all other tiny blocks. Request more tiny blocks
   after the first bunch is processed. A request may include hash of
   last processed tiny block, for example, or just epoch
   number. Download and process all available tiny blocks.
7. If current epoch is `x`, last tiny block will be for epoch
   `x`. Request headers from last `2 · l` slots of
   epoch `x - 1`.
8. Verify that headers are consecutive and issued by proper
   stakeholders. If that's the case, request the block corresponding
   to the oldest header and take hash of snapshot (`H`) from it.
9. Download snapshot somehow. The mechanism to download a snapshot is
   not described in this document. Check it's correctness according
   to `H`.
10. Download all blocks after that snapshot and verify them fully
    according to the snapshot.
11. After all recent blocks are processed, the node is fully
    functional. It still makes sense to download all other blocks to
    be 100% sure the whole chain is valid.

#### Choice of `l`

The choice of `l` affects performance requirements and security
guarantees. There is a certain trade-off.

The biggest reasonable value of `l` is `k` (security parameter of
Ouroboros protocol). It's the best value from security point of
view. However, it might be not feasible for real system for
performance reasons. Specifically, there are two problems:
1. It's not trivial to compute slot leaders for an epoch, because it
   requires iterating over whole UTXO or stakes map which might be
   big. It's not yet clear how much time it will take in practice, but
   probably not just few seconds. If it takes a minute and `l = k`, first three
   slots among last `2 · k` will be missed, because their leaders will
   be computing slot leaders (because it's mandatory to put a
   signature of this list into new block).
2. If leaders must be computed before creating a block for `8 · k`-th
   slot, it's enough to force rollback of only one block to require
   nodes to recalculate slot leaders. It's not very expensive to make
   a fork of depth 1 (or 2, 3). However, computing slot leaders is an
   expensive operation, so we'd like to avoid doing it more than once
   for epoch.

If we pick `l` slightly smaller than `k`, both problems will be
resolved with high probability. In Cardano SL `k = 2160`. Suppose we
say `l = 2100`. Then:
1. Each node will have 40 minutes to compute list of slot
   leaders. 20 minutes should be enough for everybody.
2. Rollback of 60-120 (depending on chain quality) blocks is very
   unlikely to happen, so almost always it will be enough to compute
   slot leaders only once.
3. However, we will assume that there is at least one honest block in
   2100 blocks, not in 2160. Probably it's not a big problem, because
   probability of all blocks being invalid decreases exponentially, so
   there should be significant difference between 2100 and 2160 (in
   both cases probability is negligible).

### Properties

Let's assume that `l ≈ k = 2160`.

#### Correctness of the snapshot

The probability of the snapshot being incorrect is the same as the
probability of a fork in Ouroboros protocol with depth more than
`l`. If `l` is almost the same as `k`, this probability is negligible.

Recall the steps from
[fast synchronization procedure](#fast-synchronization-procedure). The
node can easily verify tiny block for `e`, because it knows leaders
for epoch `e` (it has all blocks before `e`). For tiny block for
epoch `e + 1` it's only possible to verify that signatures are valid
and correspond to actual slot leaders, but it's impossible to verify
list of slot leaders put into that tiny block. However, it's easy to
see that if signatures are valid, list of leaders should also be valid
if Common Prefix property with parameter `l` is not violated. In this
case among last `2 · l` slots of epoch `e` there are at least `l`
blocks and at least one of them must be produced by an honest
stakeholder according to Common Prefix property. It implies that at
least one signature in the tiny block for epoch `e + 1` is produced by
an honest stakeholder, so the whole list must be correct. Verification
of subsequent tiny blocks is the same.

If all tiny blocks are correct, headers received at step 8 are
guaranteed to be created by real slot leaders. According to Common
Prefix property, at least the oldest block must be created by an
honest stakeholder, hence it must contain the hash of the correct
snapshot.

#### Data to download

Size of a serialized stakeholder identifier is 30 bytes, size of a
public key is 66 bytes and size of a simple signature is 66 bytes. In
the worst case all signatures are proxy signatures and then a tiny
block contains at most 8640 signatures and public keys and 4320
stakeholder identifiers. So the size of one tiny block can be
estimated as 1275000 bytes (considering CBOR overhead) in the worst
case.

One year contains 73 epochs. So the size of all tiny blocks is
approximately 93075000 bytes per year, which is approximately 93
megabytes. Note, however, that
some [optimizations](#possible-optiomizations) are possible at the
cost of extra complexity.

It's also necessary to download some full blocks and headers, but
their number is limited.

#### Local computations

In order to verify a tiny block one needs to check at most 8640
signatures and hash at most 4320 public keys. For one year it's needed
to check 630720 signatures and compute 315360 hashes. These
computations can be performed in less than a second. So the main
property is the size of data which should be downloaded.

### Possible optimizations

Using some optimizations it's possible to improve some properties at
the cost of making the design and/or implementation a bit more
complex. With currently proposed scheme the size of a tiny block in
the worst case scenario is about **1275** kilobytes

* More compact serialization. Instead of using CBOR for everything, we
  can serialize tiny blocks more compactly. Then a hash will take 28
  bytes, a simple signature and a public key will take 64 bytes. It
  can make a tiny block a bit smaller, approximately **1231**
  kilobytes (**90** Mb per year).
* Put less signatures into a tiny block. For instance, if all slots
  are filled, it's enough to put first `k` signatures into a tiny
  block. If some slots don't have blocks, it's probably not enough to
  put only `k` signatures, but `2 · l` is not necessary. More profound
  security analysis should be done to determine lower and upper
  boundaries. In the best case, if `k` signatures is always enough and
  the previous optimization is done, the size of a tiny block can be
  about **674** kilobytes (**49** Mb per year).
* Another option is to make `l` much smaller than `k`. It depends on
  how much trust we want to have. `k = 2160` is quite a lot. It's much
  greater than number of confirmations after a transaction is
  considered stable. Probably having `l = 500` will be enough for
  regular end users. In this case the size of a tiny block will be
  only **284** kilobytes (**21** Mb per year).
* Compression. Most likely some stakeholders will be slot leaders more
  than once. In this case it's not necessary to put their public keys,
  signatures and identifiers more than once. A simple compression can
  significantly decrease the average size of a tiny block. It's hard
  to predict exact ratio, but let's assume after compression a block
  will be 15% smaller. It will require a bit more local computations,
  but time spent on them should still be negligible. With previous
  optimizations one tiny block will take about **573** kilobytes if `l
  ≈ 2160` (**42** Mb per year) or **241** kilobytes if `l = 500` (18
  Mb per year).

If we get rid of lightweight delegation (which might happen, but is
not decided yet), the size of a tiny block in the worst case will be
approximately 1.8 times smaller. It should be less than 10 megabytes
of data to download for one-year-long blockchain if `l = 500`.

All these optimizations are relatively easy to do, but some of them
require more research or involve a certain trade-off. We can go even
further with a bit more complicated optimizations.

* Aggregate signature. Instead of putting `2 · l` or approximately `k`
  (with one optimization) signatures we can put one aggregated
  signature into a tiny block. However, it's not clear whether our
  current signature scheme supports aggregate signatures. If it
  doesn't, additional effort will be required to figure out how to use
  an alternative scheme. Additional research can be done if this
  optimization is considered useful.
* It's theoretically possible to avoid stakeholder identifiers in tiny
  blocks entirely. It should save approximately 18% of space. However,
  it might require significant changes in the protocol, because
  currently public keys of slot leaders are not revealed before they
  actually use them.

### Two notes about delegation

If we preserve heavyweight delegation, we must make a change to put
heavweight delegation into effect only in the next epoch after
certificates is put into block. Currently if there is a certificate in
block `X` from Alice to Bob, all blocks after `X` must be created by
Bob instead of Alice. But proposed solution assumes that we know who
will create blocks in all slots of epoch `e` when `e` starts. See also
CSL-2069.

It's not necessary to do it for lightweight delegation, because we
don't need to check whether lightweight delegation certificate is in
the blockchain.

Another note is related to lightweight delegation. Suppose Alice delegates
to Bob using heavyweight delegation. While this delegation is active,
Alice can't create a block. However, she can delegate to someone else
using lightweight delegation and create a block. It should be
prohibited, because it basically allows multiple stakeholders to
create a block. See also CSL-2112.

### Inclusion into Cardano SL

This section briefly describes a draft how to extend Cardano SL to
support this mechanism.

Unless we do the most complex optimizations (usage of aggregate
signatures and revealing leaders' public keys in advance), it should
be possible to introduce this mechanism via a softfork, no hardfork is
necessary. Even with those optimizations it might be possible to avoid
a hardfork.

At some point there will be an update proposal with new block version
`bv`. This block version will be adopted as result of processing of
genesis block for some epoch `e`. Since epoch `e` full nodes with
newest software will maintain a chain of tiny blocks. The first tiny
block will be created for `e + 1` (when `e + 1` starts).

All blocks created since epoch `e` must have version `bv` or newer
(it's enforced by update system). We require all blocks with version
`bv` or newer to have a signature of slot leader for the next epoch if
the block is created for one of last `2 · l` slots, as was described
earlier. This data can be put into attributes of a block. Nodes with
old software will ignore this data. Note that block header contains
hash of block attributes. Also a block with these block versions
(`bv` or newer) must contain hash of a valid snapshot if it's the
first block after the `8 · k`-th slot.

We should also include new endpoints to request tiny blocks which
shouldn't be a big problem.

### Open questions

Let's summarize all questions which should be answered before this
scheme can be implemented:
1. What should be value of `l`?
2. How to incentivize full nodes to maintain and send tiny blocks? One
   incentive is to make system more convenient to use and thus
   indirectly increase value of ADA (presumably). Is this incentive
   enough? Should be worry that full nodes may want not to do it
   (because at least it requires some storage space, after all).
3. Will we have lightweight delegation? Will it be the same as now? It
   affects sizes of tiny blocks. This scheme is related to delegation,
   so other changes in delegation might require some changes in this
   scheme or calculations.
4. Other questions can be formulated as a single question: which
   proposed optimizations do we want to consider? For instance, if we
   want to consider aggregate signature, we should figure out which
   signature scheme can be used and how to integrate it with the
   existing one. If we want to compress tiny blocks, we should choose
   appropriate compression. And so on.

### Credits

The idea was originally proposed by George Agapov. Though I don't know
if it was proposed by someone else before independently.

## Comparison and conclusion

Let's compare these two approaches:
1. The solution with checkpoints is easier to implement.
2. The solution with checkpoints allows to obtain snapshot hash faster
   than the one with tiny blocks, even if millions of blocks are
   missing. However, it's not yet clear how much time actual snapshot
   downloading will take. It might take more time than obtaining
   snapshot hash using tiny blocks approach. In this case this
   advantage is not very significant.
3. The solution with checkpoints is a bit less convenient to use
   because it requires manual interaction. Clients need to import
   trusted keys, stakeholders need to publish them somewhere.
4. The solution with checkpoints relies on trusted third parties,
   while the solution with tiny blocks has basically the same
   assumptions as Ouroboros protocol.
