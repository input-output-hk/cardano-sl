# Cardano SL Databases

## node-db

```
blocks/index: 22M
blocks/data: 374M
gState/: 11M
lrc: 2.1M
misc: 292K
```

^ sample sizes, for empty blockchain generated for number of epochs

### blocks/index

* `"b" <> headerHash` -> `BlockHeader`

### gState

#### common

* `c/tip` -> `HeaderHash`
* `init/gstate` -> `()`
* `c/bot` -> `HeaderHash` *Not updated, to be removed*
* `c/maxsd` -> `ChainDifficulty`

#### misc

* `gtSecretStorageKey` -> `SscSecretStorage` <- own generated secret, for last epoch (for which secret was generated)
* `skh/` -> *Unused, to be removed*
* `psk/` -> `[ProxySKLight]`

#### utxo

* `"ut/t/" <> txIn` -> `TxOutAux`

#### balances

* `"b/s/" <> stakeHolderId` -> `Coin`
* `b/ftssum` -> `Coin` <- total stake

#### ssc

* `ssc/` -> `SscGlobalState` <- full state of SSC

#### block extra

* `e/ls/` -> `LastBlkSlots` (description [here](https://github.com/input-output-hk/cardano-sl/blob/ac1ec4740865dffb615ce51081b7920af18576b7/node/src/Pos/Block/Slog/Types.hs#L22))
* `"e/mc/" <> headerHash` <- `()`, key is present if block with corresponding id is in main chain
* `"e/fl/" <> headerHash` <- `HeaderHash`
   * Forward link, used by block retrieval code. Unlike `prevBlock` field of block header, which points to predcessor of given block, this link will return block, which is next after given

#### update system

* `us/epoch-proposers/` -> `HashSet StakeholderId`
* `us/slotting/` -> `SlottingData` <- Data necessary for slotting to work
* `"us/cp/" <> softwareVersion` -> `ConfirmedProposalState` <- Confirmed proposals
* `"us/p/" <> upProposalId` -> `ProposalState` <- Proposals
* `"us/cv/" <> applicationName` -> `NumSoftwareVersion` <- actual (confirmed) version for application
* `"us/bvs/" <> blockVersion` -> `BlockVersionState` <- block version registry
   * For all block versions? Or confirmed? Or how?
* `us/adopted-block-version/` -> `(BlockVersion, BlockVersionData)` <- adopted BV

### LRC

#### Leaders

* `"l/" <> epochIndex` -> `SlotLeaders` a.k.a. `NonEmpty StakeholderId`

#### Richmen

* `"r/ssc/" <> epochIndex` -> `RichmenStakes` <- richmen distribution
   * with threshold for MPC participation
* `"r/us/" <> epochIndex` -> `FullRichmenData` a.k.a. `(Coin, RichmenStakes)` <- total rich stake and distribution
   * with threshold to vote for update
* `"r/dlg/" <> epochIndex` -> `RichmenSet` a.k.a. `HashSet StakeHolderId`
   * with threshold to use heavyweight dlg

#### Common

* `c/epoch` -> `EpochIndex` <- epoch up to which LRC is computed

#### Issuers

* `"i/" <> epochIndex` -> `HashMap StakeHolderId Coin` <- this map contains stakes of issuers of blocks. It is needed for update system. Suppose there are two competing block versions and there are some blocks with these competing block versions. They are issued by some stakeholders. We store stakes of stakeholders who issued these blocks (with competing versions). For `i`-th epoch we use `crucialSlot` for this epoch.

#### Seed

* `"s/" <> epochIndex` -> `Seed` <- seed for every epoch

### Explorer DB

*To be done*

## wallet-db

## secret.key
