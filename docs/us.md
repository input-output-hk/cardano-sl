# Update system consensus rules

## Prerequisites

* [Update Mechanism](https://cardanodocs.com/cardano/update-mechanism/).

## Overview
Update system gives ability to developers to propose updates of software applications and
ability to users to vote for updates to decide which one will be accepted.

This document describes a global part of update system consensus rules, as well difference between a local part.
Global consensus rules is a part of block processing, which checks validity of update payload of blocks and update 
corresponding state when blocks are applied/rolled back. 
We first describe global part and then describe how local part differs.

## Problem definition
TBA

## Update system model

### Poll and decision agreement rules

Each stakeholder can send update proposal in the blockchain,
and others stakeholders can send their votes "for" or "against" it.
Making decision whether approve or reject an update proposal depends on sum of stake "for" and "against" it.

There are two rules to make decision about update proposal:
* **Explicit agreement**: if an update proposal has greater than 50% stake "for" or "against" 
then the proposal becomes approved or rejected corresponding.
* **Implicit agreement**: if an update proposal has been proposed in the blockchain `U` slots ago,
and it has more stakes "for" then "against", then the proposal becomes approved and rejected otherwise.

### Update proposal states

Update proposal may be in three states:
* **Active** (or Undecided)  
When an update proposal gets into the blockchain within some block, it becomes _active_.
This state means that the proposal is known in the blockchain but 
it wasn't still decided by stakeholders whether to approve or to reject this proposal.
So, a poll is active and stakeholders' votes which get into the blockchain affect the decision.

* **Decided**  
An update proposal continues being _active_ until it becomes _decided_, it may be _decided_ to approve
or to reject.
If a proposal is rejected then it doesn't affect any consensus rules and we don't consider it anymore.
Note: rejected proposal may become active again if rollback occurs.

* **Confirmed**  
If a proposal has been approved in some block and there are at least `k` blocks after this one, then
update proposal is called _confirmed_.
_Confirmed_ state reflects the fact that a proposal's state cannot be changed anymore 
because we have guarantee that at most `k` blocks may be rolled back.

### Software and block versions. Data associated with block version

When a developer wants to fix some bug or add some features to application,
which don't affect protocol rules, he should propose an update with new `SoftwareVersion`.
```
data SoftwareVersion = SoftwareVersion
    { svAppName :: !ApplicationName
    , svNumber  :: !NumSoftwareVersion
    }
```

`SoftwareVersion` contains application name and numeric representation of application version.

If a developer wants to change some protocol rules, for example, `Address` format, 
he must propose update with new `BlockVersion`

```
data BlockVersion = BlockVersion
    { bvMajor :: !Word16
    , bvMinor :: !Word16
    , bvAlt   :: !Word8
    }
```

So block version essentially is a tuple `(Maj, Min, Alt)`.
Block version is also called **protocol version**.

There are some protocol constants which depend on `BlockVersion`, for example max block size, slot duration, etc.
Update system was designed in such way that you must attach new values of this constants if they are updated.
`BlockVersionModifier` reflects these changes:
```
data BlockVersionModifier = BlockVersionModifier
    { bvmScriptVersion     :: !(Maybe ScriptVersion)
    , bvmSlotDuration      :: !(Maybe Millisecond)
    , bvmMaxBlockSize      :: !(Maybe Byte)
    , bvmMaxHeaderSize     :: !(Maybe Byte)
    , bvmMaxTxSize         :: !(Maybe Byte)
    , bvmMaxProposalSize   :: !(Maybe Byte)
    , bvmMpcThd            :: !(Maybe CoinPortion)
    , bvmHeavyDelThd       :: !(Maybe CoinPortion)
    , bvmUpdateVoteThd     :: !(Maybe CoinPortion)
    , bvmUpdateProposalThd :: !(Maybe CoinPortion)
    , bvmUpdateImplicit    :: !(Maybe FlatSlotId)
    , bvmSoftforkRule      :: !(Maybe SoftforkRule)
    , bvmTxFeePolicy       :: !(Maybe TxFeePolicy)
    , bvmUnlockStakeEpoch  :: !(Maybe EpochIndex)
    }
```
So if a value is going to be updated then a field is `Just` and `Nothing` otherwise.

### Adoption of block version

Assume a proposal which is bumping `BlockVersion` became _confirmed_.
Though the proposal is already _confirmed_ along with block version but stakeholders
haven't updated its software yet, hence, they can't validate block of _confirmed_ block version.

To avoid a situation when adversary stakeholder issues invalid block of just _confirmed_ block version,
others stakeholders will validate blocks according to previous **adopted** block version, 
until _confirmed_ becomes **adopted**.

Informally `BlockVersion` is **adopted** if sum of block issuers' stakes, 
which issued blocks of this `BlockVersion` at least once, is a majority.

Formally, let's say a proposal became _confirmed_ in `s` epoch and current epoch is `t`:  
if portion of block issuers' stakes, which issued blocks of this `BlockVersion` at least once, is greater than
`max spMinThd (spInitThd - (t - s) * spThdDecrement)`, then proposal's `BlockVersion` becomes **adopted**.

Intuitively, the threshold which needed to adopt `BlockVersion` is decreasing in each epoch but cannot become
less than some reasonable minimal value (`spMinThd`).

There is datatype which can also be updated by `BlockVersionModifier`:
```
data SoftforkRule = SoftforkRule
    { srInitThd      :: !CoinPortion
    , srMinThd       :: !CoinPortion
    , srThdDecrement :: !CoinPortion
    }
```

So we check this rule at the beginning of each epoch for each _confirmed_ proposal 
and adopt one of this _confirmed_ `BlockVersion` if it satisfies the rule.

Note: last adopted block version is the feature of the blockchain, not stakeholders' software or something else.

#### Example

Assume there are four stakeholders with stakes `[0.25, 0.26, 0.1, 0.39]`,
`lastKnownBlockVersion` is `0.1.0` for all stakeholders' softwares and 
last adopted block version is also `0.1.0`.

Then update proposal which bumps block version to `0.2.0` is confirmed in 2nd epoch.
Suppose this proposal adds consensus rule, 
that block header's attributes should contain by key `228` string `"pva"`.

Then the first stakeholder downloads this update and update own software. 
`lastKnownBlockVersion` is `0.2.0` for this software but last adopted block version is still `0.1.0`.

Then the first stakeholder issues a block as leader of some slot: 
* `BlockVersion` of this block is set to `0.2.0`
* block created by rules for last adopted version, i.e. `0.1.0`. 
Update software considers last adopted version and creates block using old logic 
if this one is `0.1.0` and using new logic if this one is `0.2.0`.
So the block is issued without new attribute. Stakeholders with old software expects that all attributes are known because
last adopted version equals `lastKnownBlockVersion` for its softwares.

Then the 3rd epoch starts and the threshold (which is regulated by `SoftforkRule`) 
for adopting new block version is dumped to `0.6`.

Then the second stakeholder downloads this update and when it's turn to issue block, 
the stakeholder issues block like it has been done by the first stakeholder.
Also the second one issues one more block as leader, so it produced 2 blocks.

Then 4th epoch starts and sum of issuers stakes is `0.25 + 0.26 = 0.51 < 0.6` 
(we take into account the second stakeholder only once). Adopting threshold is dumped to the minimal value `0.6`.
So last adopted version is still `0.1.0`, `lastKnownBlockVersion` is `0.2.0` for 
the first and the second stakeholders and `0.1.0` for the third and the fourth.

Then the third stakeholder downloads this update and issues block in the same manner like previous ones.

When 5th epoch starts the sum of issuers stakes is `0.25 + 0.26 + 0.1 = 0.61 > 0.6` and 
it's time to bump last adopted version.
So last adoped version is `0.2.0` now and `lastKnownBlockVersion` is `0.1.0` 
for the latter stakeholder and `0.2.0` for others.

When it's turn to issue block of the fourth stakeholder, 
it issues block with `0.1.0` block version and with block format for `0.1.0`.
Others stakeholders see that last adopted version equals `0.2.0` and 
try to decode block using new rules but failed because it doesn't contain attribute with key `228`.
So this slot left without a block.

You can see despite the provided update system is powerful, it's also very complicated 
and updates should be handled carefully.

## GState

## Verification
