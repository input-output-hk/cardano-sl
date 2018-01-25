# Reaction on rollback

This document describes how one should react to rollbacks.

## Overview

Rollback happens when node's blockchain `A` is not a prefix of another
valid blockchain `B` which has more blocks than `A`. In this case node
will rollback all blocks from `A` which are not in `B` and apply all
blocks from `B` which are not in `A`.  Rollback itself doesn't mean
that the system is broken. It's an integral part of many, if not all,
cryptocurrencies. However, if all stakeholders are honest, always
online, don't have bugs and there are no delays (e. g. networking),
rollbacks are impossible.  In real world only the first condition is
satisfied in Byron (all stakeholders are indeed honest). And it
significantly decreases chances of rollback, but not fully.

Examples:
1. Node `N` creates a block `B` and is immediately stopped before it
   announces it to other nodes. After the node is started again, other
   nodes will create more blocks and `N` will have to rollback `B`.
2. Another example: there is a huge delay in blocks propagation
   (unstable network) and because of that some node may not receive
   block for slot `i` before it ends. And then create block for slot
   `i + 1` (not based on `i`). It will also lead to rollback.

## How to notice rollback

There are several ways to notice that rollback occurred:
1. Messages like these:
  * > Blocks to rollback [a295e975]
  * > Blocks have been rolled back: [a295e975]
  * > Trying to apply blocks w/ rollback:
2. When rollback happens, node reports it to reporting server. If
   rollback is deep (determined by a constant from configuration),
   misbehaviour is classified as critical.
3. In CSL-2014 we want to add EKG metrics for each misbehaviour, in
   particular for rollbacks (PR https://github.com/input-output-hk/cardano-sl/pull/2314 was merged).

## How to classify rollback

The most important value is the depth of rollback, i. e. how many
blocks are rolled back. `Blocks to rollback` and `Blocks have been
rolled back` messages contain hashes of blocks, so one can easily
calculate them. Reports sent to reporting server also contain hashes
of blocks. EKG metric (CSL-2014) will contain depth.

Other essential values are: how often rollback happens and how many
nodes experience it.

Normally, rollbacks may have depth 1 or 2 and happen at most 1 or 2
times per day. They shouldn't affect several nodes at once. If
rollback has greater depth, happens more often or affects several
nodes at once, it should be investigated. If network is overloaded,
nodes are often restarted, there are delays in network communication
and other infrastructural issues, they can lead to more often or deep
rollbacks.

So there are the following classifications:
1. Rollback depth.
  * If it's less than 3, rollback is shallow.
  * If it's greater than 2 and less than 21, rollback is deep.
  * If it's greater than 20, rollback is very deep.
TODO: I don't know exchanges' assumptions. It should be clarified!
I've asked in relevant channel.
2. Rollback frequency.
  * If rollbacks happen more than once in 10 minutes, we consider
    these rollbacks frequent.
  * Otherwise we consider them occasional.
3. Number of affected nodes.
  * If more than half of core nodes make rollback in a short period of
    time (less than a minute), we consider this rollback global.
  * Otherwise it's local.

## How to react to rollbacks

As was said earlier rollback doesn't necessary mean that the system
misbehaves and some properties are violated. But we still need to care
about them, because:
1. Exchanges and end users assume that after certain number (`n`) of
   confirmations (i. e. blocks) their transaction is stable. If
   rollback of more than `n` blocks happens, transaction may be
   reverted. Note, however, that rollback may happen only for one
   node, and in this case it won't be misbehaviour visible to
   exchanges or end users.
2. As a precaution we should also inspect not very deep rollbacks,
   because they _might_ indicate a problem.

You should [classify rollback](#how-to-classify-rollback): how deep
and frequent it is and how many nodes are affected.
* If rollback is shallow, occasional and local, ignore it.
* If rollback is very deep, bring developers' attention immediately
and provide logs near the time when very deep rollback
happened. Optionally proceed to the next step (you can leave it to
developers).
* Otherwise let developers know that rollback happened, ideally by
  creating a YouTrack issue with relevant logs and timestamps, wait
  for their response.
