# Relating the Wallet Specification To Cardano

This document is pinned to these versions of the Cardano code and Wallet Spec:

- **Cardano code**: Git commit 6659d8501c727714a7861ad2e527a337e0a11b86
- **Wallet Spec**: v1.2 (12 July 2018)

## Introduction

This document relates the Wallet Spec to both the DSL and Cardano implementations.

### The "DSL"

The DSL resembles the Wallet Spec very closely and ignores many details and complexities that the Cardano code must contend with.

Moreover, many of the invariants defined in the Wallet Spec are encoded (and tested) on the DSL rather than in Cardano.
(see "DSL and Wallet Spec Invariants" below).

Generative tests exhaustively compare the DSL and Cardano implementations and give assurance that the Cardano code matches the DSL implementation, which in turn very closely matches the Wallet Spec. (more on this [here](https://www.well-typed.com/blog/2018/05/semi-formal-development/))

### What's not in Cardano

Cardano does not implement the entire Wallet Spec, for instance *expected UTxO* and associated properties like *minimum balance* are not currently supported (and are not represented in the DSL model either).

### How to read this document alongside the Wallet Spec

After describing how the Cardano Wallet relates to the Wallet in the Spec, we will follow the Wallet Spec sections and show the corresponding Cardano code and concepts.

The Wallet Spec defines a wallet in layers, incorporating one new idea at a time (e.g. prefiltering, rollback, efficiency, metadata). Only in Fig 11 of the Spec do we reach the full definitions of the wallet as it is implemented in Cardano.

For example `applyBlock` is first defined in Fig 3, then Fig 6 (with prefiltering incorporated), Fig 7 (in the presence of rollback), Fig 8 (with expected UTxO), Fig 10 (full model), and finally Fig 11 (which also considers metadata).

In this document, we show the corresponding Cardano code for concepts in the sections where they are first treated in detail, e.g. we include the Cardano definitions of `applyBlock` in the section on prefiltering, which is well before `applyBlock` is fully defined in the spec (see Section 10 - Tracking Metadata).

### The Cardano Wallet

A Cardano Wallet has a

* PassiveWallet (passive because it cannot submit transactions)
* access to the diffusion layer
* and write access to the submission layer in the `PassiveWallet`

```haskell
-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive       :: PassiveWallet
      -- | The wallet diffusion layer
    , walletDiffusion     :: WalletDiffusion
      -- | The protocol magic used to make transactions.
    , walletProtocolMagic :: ProtocolMagic
    }
```

[src](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Internal.hs#L96-L107)

A `PassiveWallet` has a

* KeyStore which provides access to the HD wallet root keys
* Wallet state store: `AcidState DB`
* Transaction metadata store: `sqlite`
* The `NodeStateAdaptor` provides access to the underlying node, primarily used for restoration (when the wallet's state is not available)
* partial write access to the submission layer (while the "passive" wallet cannot send transactions, it does need to de/register transactions with the submission layer)

```haskell
data PassiveWallet = PassiveWallet {
      _walletLogMessage :: Severity -> Text -> IO ()
    , _walletKeystore   :: Keystore
    , _wallets          :: AcidState DB
    , _walletMeta       :: MetaDBHandle
    , _walletNode       :: NodeStateAdaptor IO
    , _walletSubmission :: MVar WalletSubmission
    }

```
[PassiveWallet](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Internal.hs#L47-L87)

#### Wallet State

The HD wallet hierarchies are "flattened" into `IxSet` collections of

* HD roots
* Accounts
* Addresses

This allows for direct access Accounts and Addresses, without having to navigate the HD hierarchies.

**DSL**

```haskell
data State h a = State {
      _stateUtxo    :: Utxo h a
    , _statePending :: Pending h a
    }
```

[Basic Model](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/test/unit/Wallet/Basic.hs#L33-L36)
/
[Incremental Model](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/test/unit/Wallet/Incremental.hs#L42-L45)
/
[Basic Model, with rollback](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/test/unit/Wallet/Rollback/Basic.hs#L31-L34)
/
[Full Model, with rollback](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/test/unit/Wallet/Rollback/Basic.hs#L31-L34)

**CARDANO**

```haskell
data DB = DB {
      _dbHdWallets :: !HdWallets
    }

data HdWallets = HdWallets {
    _hdWalletsRoots     :: !(IxSet HdRoot)
  , _hdWalletsAccounts  :: !(IxSet HdAccount)
  , _hdWalletsAddresses :: !(IxSet (Indexed HdAddress))
  }
```
[DB](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/AcidState.hs#L75-L88)
/
[HdWallets](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/HdWallet.hs#L559-L567)

Each Cardano Wallet _Account_ corresponds to a _Wallet_ in the Wallet Spec.

```haskell
data HdAccount = HdAccount {
		...
    , _hdAccountState         :: !HdAccountState
	   ...
    }

data HdAccountState =
      HdAccountStateUpToDate !HdAccountUpToDate
    | HdAccountStateWithinK  !HdAccountWithinK
    | HdAccountStateOutsideK !HdAccountOutsideK

-- | Account state for an account which has complete historical data
data HdAccountUpToDate = HdAccountUpToDate {
      _hdUpToDateCheckpoints :: !(NewestFirst StrictNonEmpty Checkpoint)
    }
```
[src](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/HdWallet.hs#L286-L327)

An Account's state, a list of checkpoints, corresponds to _Wallet State_ in the Wallet Spec.

```haskell
data Checkpoint = Checkpoint {
      _checkpointUtxo        :: !(InDb Core.Utxo)
    , _checkpointUtxoBalance :: !(InDb Core.Coin)
    , _checkpointPending     :: !Pending
    , _checkpointBlockMeta   :: !BlockMeta
    , _checkpointSlotId      :: !(InDb Core.SlotId)
    , _checkpointForeign     :: !Pending
    }
```
[src](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec.hs#L65-L100)

During `applyBlock`, a new checkpoint is added. `newPending` amends the current checkpoint and `rollback` reverts to previous checkpoint data.

### Core API Architecture

The core wallet API is partitioned into Updates and Queries.

#### Wallet Updates

Note:

* `applyBlock` (and `switchToFork`) take the `PassiveWallet` as argument, whereas `newPending` requires the `ActiveWallet` since it need access to the submission and diffusion layers.

* `applyBlock` expects resolved blocks, not core cardano blocks (see "Prefiltering in Cardano")
* analogously, `newPending` requires for transaction metadata to already be prepared

##### Core Update API

**BListener.hs** `applyBlock`, `switchToFork`, `newPending`

```haskell
applyBlock :: PassiveWallet -> ResolvedBlock -> IO ()
```
[BListener.applyBlock](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/BListener.hs#L54-L64)

**Pending.hs**

* `newPending`, `newForeign`

e.g.

```haskell
newPending :: ActiveWallet
           -> HdAccountId
           -> TxAux
           -> Maybe TxMeta
           -> IO (Either NewPendingError ())
```
[BListener.newPending/newForeign](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Pending.hs#L40-L66)

##### AcidState layer

Note:

* each wallet operation is expressed as an atomic update in the AcidState layer (e.g. `applyBlock` below)
* at this layer the wallet operations require prefiltered blocks (that are indexed by HD wallet account)
* `applyBlock` will be applied to *every* account in *every* local HD wallet, similary `switchToFork` is applied to all local accounts
* on the other hand, `newPending` is performed on a particular account (but may well affect the transaction metadata of other accounts)

```haskell
applyBlock :: SecurityParameter
           -> InDb SlotId
           -> Map HdAccountId PrefilteredBlock
           -> Update DB (Map HdAccountId (Set TxId))
```
[AcidState.applyBlock](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/AcidState.hs#L186-L248)

##### Core Wallet Spec Implementation (Updates)

The core implementations of Cardano wallet updates consist of  pure functions that act on single account checkpoints.

```haskell
applyBlock :: SecurityParameter
           -> SlotId
           -> PrefilteredBlock
           -> NewestFirst StrictNonEmpty Checkpoint
           -> (NewestFirst StrictNonEmpty Checkpoint, Set Txp.TxId)
```
[Spec.Update.applyBlock/Partial](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L148-L200)

#### Wallet Reads

##### Query API

```haskell
currentAvailableUtxo :: DB -> HdAccountId -> Either UnknownHdAccount Utxo
currentAvailableUtxo = liftHd1 HD.currentAvailableUtxo

currentTotalBalance :: DB -> HdAccountId -> Either UnknownHdAccount Coin
currentTotalBalance = liftHd1 HD.currentTotalBalance

```
[src](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Read.hs#L95-L99)

##### AcidState layer

There is only one AcidState query, `snapshot`, that reads the whole DB. This is why queries can be pure functions (that read from a DB provided as argument).

```haskell
snapshot :: Query DB DB
snapshot = ask
```
[snapshot](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/AcidState.hs#L468-L472)

For each read there is a corresponding pure function that runs in the Query' monad, e.g.

```haskell
currentAvailableUtxo :: HdAccountId -> Query' HdWallets UnknownHdAccount Utxo
```
[currentAvailableUtxo](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/HdWallet/Read.hs#L131-L132)

##### Core Wallet Spec Implementation (Reads)

These above pure functions rely on the core implementation of wallet reads as defined in the Wallet Spec.

e.g.

```haskell
cpAvailableUtxo :: IsCheckpoint c => c -> Core.Utxo
```
[cpAvailableUtxo](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Read.hs#L40)

At this layer the (pure) functions on defined checkpoints (which belong to an HD wallet account).

## Section 2 - Preliminaries

**Fig 1 - Primitive Types**

**DSL**

```haskell
-- transaction id

Int

-- index

type AddrIx = Int

-- address

data ActorIx
  = IxRich Int
  | IxPoor Int
  | IxAvvm Int

data Addr = Addr {
      addrActorIx :: ActorIx
    , addrIx      :: AddrIx
    }

-- currency value

type Value = Word64
```
[AddrIx,ActorIx,Addr](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/test/unit/UTxO/Context.hs)

**CARDANO**

```haskell
-- transaction id

type TxId = Hash Tx

-- index

--- (Word32)

-- address

data Address = Address
    { addrRoot       :: !(AddressHash Address')
    , addrAttributes :: !(Attributes AddrAttributes)
    , addrType       :: !AddrType
    }

-- Address in an account of a HD wallet
data HdAddress = HdAddress {
      _hdAddressId      :: !HdAddressId
    , _hdAddressAddress :: !(InDb Core.Address)
    }

data HdAddressId = HdAddressId {
      _hdAddressIdParent :: !HdAccountId
    , _hdAddressIdIx     :: !HdAddressIx
    }

newtype HdAddressIx = HdAddressIx { getHdAddressIx :: Word32 }

-- currency value

newtype Coin = Coin
    { getCoin :: Word64
    }
```

[TxId](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/core/src/Pos/Core/Txp/Tx.hs#L130)
/
[Address](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/core/src/Pos/Core/Common/Address.hs#L118-L128)
/
[HdAddress](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/HdWallet.hs#L305-L312)
/
[HdAddressId](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/HdWallet.hs#L239-L244)
/
[HdAddressIx](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/HdWallet.hs#L141-L143)
/
[Coin](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/core/src/Pos/Core/Common/Coin.hs#L42-L45)

**Fig 1 - Derived Types**

**DSL**

```haskell
-- transaction

data Transaction h a = Transaction {
      trFresh :: Value
    , trIns   :: Set (Input h a)
    , trOuts  :: [Output h a]
    , trFee   :: Value
    , trHash  :: Int
chain.
    , trExtra :: [Text]
    }

-- transaction input

data Input h a = Input
    { inpTrans :: h (Transaction h a)
    , inpIndex :: Index
    }

-- transaction output

data Output (h :: * -> *) a = Output {
      outAddr :: a
    , outVal  :: Value
    }
  deriving (Eq, Ord)

-- unspent transaction outputs

newtype Utxo h a = Utxo { utxoToMap :: Map (Input h a) (Output h a) }

-- block

type Block h a = OldestFirst [] (Transaction h a)

-- pending transactions

type Pending h a = Map (h (Transaction h a)) (Transaction h a)

```
[Transaction,Input,Output,Block](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/test/unit/UTxO/DSL.hs)
/
[Pending](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/test/unit/Wallet/Abstract.hs#L43)

**CARDANO**

```haskell
-- transaction

data Tx = UnsafeTx
    { _txInputs     :: !(NonEmpty TxIn)  -- ^ Inputs of transaction.
    , _txOutputs    :: !(NonEmpty TxOut) -- ^ Outputs of transaction.
    , _txAttributes :: !TxAttributes     -- ^ Attributes of transaction
    }

-- transaction input

data TxIn
    = TxInUtxo TxId Word32
    | TxInUnknown !Word8 !ByteString

-- transaction output

data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    }

-- unspent transaction outputs

type Utxo = Map TxIn TxOutAux

-- block

type Block = Either GenesisBlock MainBlock

-- pending transactions

type UnderlyingMap = Map Core.TxId Core.TxAux

newtype Pending = Pending (InDb UnderlyingMap)

-- filtered sets

-- (see "Prefiltering")

```
[Tx](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/core/src/Pos/Core/Txp/Tx.hs#L57-L93)
/
[TxIn](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/core/src/Pos/Core/Txp/Tx.hs#L145-L193)
/
[TxOut](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/core/src/Pos/Core/Txp/Tx.hs#L215-L238)
/
[Utxo](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/chain/src/Pos/Chain/Txp/Toil/Types.hs#L53)
/
[Block](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/chain/src/Pos/Chain/Block/Union/Types.hs#L446)
/
[Pending](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Pending.hs#L58-L62)

**Fig 2 - Wallet Spec**

(refer to later re-definitions)

## Section 3 - The Basic Model

### DSL and Wallet Spec Invariants
The DSL testing infrastructure also asserts the relevant invariants specified in the Wallet Spec.

```haskell
-- Invariant 3.4
pendingInUtxo :: (Hash h a, Buildable a) => WalletInv h a
pendingInUtxo l e = invariant (l <> "/pendingInUtxo") e $ \w ->
    checkSubsetOf ("txIns (pending w)",
                    txIns (pending w))
                  ("utxoDomain (utxo w)",
                    utxoDomain (utxo w))
-- Invariant 3.5
utxoIsOurs :: (Hash h a, Buildable a) => WalletInv h a
utxoIsOurs l e = invariant (l <> "/utxoIsOurs") e $ \w ->
    checkAllSatisfy ("isOurs",
                      ours w . outAddr)
                    ("utxoRange (utxo w)",
                      utxoRange (utxo w))

-- Invariant 3.6
changeNotAvailable :: (Hash h a, Buildable a) => WalletInv h a
changeNotAvailable l e = invariant (l <> "/changeNotAvailable") e $ \w ->
    checkDisjoint ("utxoDomain (change w)",
                    utxoDomain (change w))
                  ("utxoDomain (available w)",
                    utxoDomain (available w))

-- Lemma 3.8
changeNotInUtxo :: (Hash h a, Buildable a) => WalletInv h a
changeNotInUtxo l e = invariant (l <> "/changeNotInUtxo") e $ \w ->
    checkDisjoint ("utxoDomain (change w)",
                    utxoDomain (change w))
                  ("utxoDomain (utxo w)",
                    utxoDomain (utxo w))

-- Lemma 3.9
changeAvailable :: (Hash h a, Buildable a, Eq a) => WalletInv h a
changeAvailable l e = invariant (l <> "/changeAvailable") e $ \w ->
    checkEqual ("change w `utxoUnion` available w" ,
                 change w `utxoUnion` available w)
               ("total w",
                 total w)

-- Lemma 4.2
balanceChangeAvailable :: (Hash h a, Buildable a) => WalletInv h a
balanceChangeAvailable l e = invariant (l <> "/balanceChangeAvailable") e $ \w ->
    checkEqual ("utxoBalance (change w) + utxoBalance (available w)",
                 utxoBalance (change w) + utxoBalance (available w))
               ("utxoBalance (total w)",
                 utxoBalance (total w))

-- Invariant 6.3
pendingInputsDisjoint :: (Hash h a, Buildable a) => WalletInv h a
pendingInputsDisjoint l e = invariant (l <> "/pendingInputsDisjoint") e $ \w ->
    asum [ checkDisjoint ("trIns " <> pretty h1, trIns tx1)
                         ("trIns " <> pretty h2, trIns tx2)
         | (h1, tx1) <- Map.toList $ pending w
         , (h2, tx2) <- Map.toList $ pending w
         , h1 /= h2
         ]
```
[Invariants](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/test/unit/Wallet/Inductive/Invariants.hs#L197-L267)

### available

```haskell
cpAvailableUtxo :: IsCheckpoint c => c -> Core.Utxo
cpAvailableUtxo c =
    Core.utxoRemoveInputs (c ^. cpUtxo) pendingIns
  where
    pendingIns = Pending.txIns (c ^. cpPending)
```
[cpAvailableUtxo](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Read.hs#L37-L44)

where

**Fig 3 - Wallet Spec**

| Spec     	   | Code       			 |
| ------------- | ------------------ |
| txins pending | pendingIns         |
| </\|          | utxoRemoveInputs   |
| utxo          | c ^. cpUtxo        |


### updatePending

Note: for a `PrefilteredBlock` all intputs and outputs are restricted to "ours".

```haskell
updatePending :: PrefilteredBlock -> Pending -> (Pending, Set Txp.TxId)
updatePending PrefilteredBlock{..} = Pending.removeInputs pfbInputs

removeInputs :: Set Core.TxIn -> Pending -> (Pending, Set Core.TxId)
removeInputs usedInputs (toMap -> p) =
    let (pToKeep, pToEvict) = Map.partition shouldKeep p
    in (fromMap pToKeep, Map.keysSet pToEvict)
  where
    shouldKeep :: Core.TxAux -> Bool
    shouldKeep tx = Util.disjoint (Core.txIns tx) usedInputs
```
[updatePending](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L312-L316)
/
[removeInputs](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Pending.hs#L160-L175)

where

**Fig 3 - Wallet Spec**

| Spec    | Code       					|
| ------- | ---------- 					|
| txins b | pfbInputs 					|


## Section 4 - Caching Balance

**State invariant, Queries**
(refer to later re-definitions for the rest)

### availableBalance

```haskell
cpAvailableBalance :: IsCheckpoint c => c -> Core.Coin
cpAvailableBalance c =
    fromMaybe subCoinErr balance'
  where
    pendingIns   = Pending.txIns (c ^. cpPending)
    spentUtxo    = Core.utxoRestrictToInputs (c ^. cpUtxo) pendingIns
    spentBalance = Core.unsafeIntegerToCoin $ Core.utxoBalance spentUtxo
    balance'     = Core.subCoin (c ^. cpUtxoBalance) spentBalance
    subCoinErr   = error "cpAvailableBalance: spent more than available?"
```
[cpAvailableBalance](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Read.hs#L54-L63)

where

**Fig 5 - Wallet Spec**

| Spec     		| Code       		 		|
| ------   		| --------- 		 		|
| txins pending  | pendingIns     		|
| utxo           | c ^. cpUtxo          |
| <\|            | utxoRestrictToInputs |
| availableBalance | balance'     		|

### totalBalance

```haskell
cpTotalBalance :: IsCheckpoint c => IxSet (Indexed HdAddress) -> c -> Core.Coin
cpTotalBalance ours c =
    Core.unsafeAddCoin availableBalance changeBalance
  where
    availableBalance = cpAvailableBalance c
    changeBalance    = Core.unsafeIntegerToCoin $
                         Core.utxoBalance (cpChange ours c)
```
[cpTotalBalance](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Read.hs#L77-L84)

where

**Fig 5 - Wallet Spec**

| Spec     		   | Code       	    	|
| ------   		   | --------- 			|
| availableBalance  | availableBalance 	|
| change pending    | Core.utxoBalance (cpChange ours c) |
| balance           | changeBalance     |
| totalBalance      | cpTotalBalance    |


## Section 5 - Prefiltering

### Prefiltering in Cardano

#### Resolved Blocks

A transaction input refers to an output index, but not the actual spent output address and coin. During applyBlock and switchToFork, in order to verify whether an input is indeed our, we need to know the spent output address.

A `ResolvedInput` captures the corresponding "spent output" for an input. A `ResolvedTx` has "resolved" inputs and a ResolvedBlock has resolved  transactions.

* in [bracketPassiveWallet](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/WalletLayer/Kernel.hs#L42) we transform a `Blund` to a `ResolvedBlock` (see `blundToResolvedBlock`), which we pass as arg to `applyBlock` and `switchToFork` instead of a `Blund`.

```haskell
type ResolvedInput = Core.TxOutAux

data ResolvedTx = ResolvedTx {
      _rtxInputs  :: InDb (NonEmpty (Core.TxIn, ResolvedInput))
    , _rtxOutputs :: InDb Core.Utxo
    , _rtxMeta    :: InDb Meta
    }

data ResolvedBlock = ResolvedBlock {
      _rbTxs    :: ![ResolvedTx]
    , _rbSlotId :: !SlotId
    , _rbMeta   :: !Timestamp
    }

```
[ResolvedInput, ResolvedTx](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Resolved.hs#L41-L62)
/
[ResolvedBlock](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Resolved.hs#L92-L106)

#### Prefiltered Blocks

```haskell
type AddrWithId = (HdAddressId,Address)

data PrefilteredBlock = PrefilteredBlock {
      -- | Relevant inputs
      pfbInputs  :: !(Set TxIn)

      -- | Relevant outputs
    , pfbOutputs :: !Utxo

	 ...
    }
```
[AddrWithId,PrefilteredBlock](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/PrefilterTx.hs#L48-L68)

To filter a ResolvedBlock for transactions that are "ours", we discover the AccountIx and AddressIx for each output address (by decrypting the transaction output attributes).
This places the output address in a HD wallet hierarchy (Wallet/Account/Address).

When we filter a ResolvedBlock for "our" inputs and outputs, we might discover addresses that are in any number of our wallet accounts.
This is why `prefilterBlock` returns a map of `PrefilteredBlock` filtered by `HdAccountId`.

```haskell
prefilterBlock' :: PassiveWallet
                -> ResolvedBlock
                -> IO ((SlotId, Map HdAccountId PrefilteredBlock), [TxMeta])
```
[prefilterBlock'](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/BListener.hs#L39-L52)

When a block is applied to a wallet, we need to apply the prefiltered blocks to their corresponding wallet accounts. Moreover, when a block is applied and their are no corresponding entries for an account, it is still important that we apply an empty prefiltered block to that account so that an empty checkpoint will mark the passing of another block. This is also the case for `switchToFork`.

(see wallet Spec 6.6 - Ommitting checkpoints)

### updateUTxo/Balance

Note:

* for a `PrefilteredBlock` all intputs and outputs are restricted to "ours"
* `updateUtxo` computes utxo' and sigma' in the `applyBlock'` definition

```haskell
updateUtxo :: PrefilteredBlock -> (Utxo, Core.Coin) -> (Utxo, Core.Coin)
updateUtxo PrefilteredBlock{..} (utxo, balance) =
    (utxo', balance')
  where
    utxoUnion = Map.union utxo pfbOutputs
    utxoMin   = utxoUnion `Core.utxoRestrictToInputs` pfbInputs
    utxo'     = utxoUnion `Core.utxoRemoveInputs`     pfbInputs
    balance'  = Core.unsafeIntegerToCoin $
                    Core.coinToInteger balance
                  + Core.utxoBalance pfbOutputs
                  - Core.utxoBalance utxoMin

```
[updateUtxo](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L295-L310)

where

**Fig 6 - Wallet Spec**

| Spec     | Code       				  |
| ------   | --------- 					  |
| txins_b  | pfbInputs 					  |
| utxo+    | pfbOutputs 				  |
| utxo-    | utxoMin    				  |
| utxo'    | utxo'      				  |
| sigma'   | balance'   				  |
| <\|      | Core.utxoRestrictToInputs |
| </\|     | Core.utxoRemoveInputs     |


### applyBlock

```haskell
applyBlock :: SecurityParameter
           -> SlotId
           -> PrefilteredBlock
           -> NewestFirst StrictNonEmpty Checkpoint
           -> (NewestFirst StrictNonEmpty Checkpoint, Set Txp.TxId)
applyBlock (SecurityParameter k) slotId pb checkpoints = (
      takeNewest k $ NewestFirst $ Checkpoint {
          _checkpointUtxo        = InDb utxo'
        , _checkpointUtxoBalance = InDb balance'
        , _checkpointPending     = pending'
        , _checkpointBlockMeta   = blockMeta'
        , _checkpointSlotId      = InDb slotId
        , _checkpointForeign     = foreign'
        } SNE.<| getNewestFirst checkpoints
    , Set.unions [rem1, rem2]
    )
  where
    current           = checkpoints ^. currentCheckpoint
    utxo              = current ^. checkpointUtxo        . fromDb
    balance           = current ^. checkpointUtxoBalance . fromDb
    (utxo', balance') = updateUtxo      pb (utxo, balance)
    (pending', rem1)  = updatePending   pb (current ^. checkpointPending)
    blockMeta'        = updateBlockMeta pb (current ^. checkpointBlockMeta)
    (foreign', rem2)  = updatePending   pb (current ^. checkpointForeign)
```
[applyBlock/Partial](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L148-L200)

where

**Fig 7/11 - Wallet Spec**

| Spec     		| Code       			  |
| ------   		| --------- 			  |
| updateUTxo  	| updateUtxo			  |
| updatePending  | updatePending		  |
| updateExpected | (not implemented)  |

NOTE:

* for `updatePending`, see Section 3 - Basic Model
* for `updateBlockMeta`, see Section 9 - Tracking Metadata

### newPending

**Fig 7/11 - Wallet Spec**

```haskell
-- Update.hs
newPending :: forall c. IsCheckpoint c
           => InDb Txp.TxAux
           -> Update' (NewestFirst NonEmpty c) NewPendingFailed ()
newPending (InDb tx) = do
    checkpoints <- get
    let (_available, unavailable) =
           cpCheckAvailable (Core.txIns tx) (checkpoints ^. currentCheckpoint)
    if Set.null unavailable
      then put $ insertPending checkpoints
      else throwError $ NewPendingInputsUnavailable (InDb unavailable)
  where
    insertPending :: NewestFirst NonEmpty c -> NewestFirst NonEmpty c
    insertPending = currentPending %~ Pending.insert tx

-- Pending.hs
insert :: Core.TxAux -> Pending -> Pending
insert tx = liftMap $ Map.insert (hash (Core.taTx tx)) tx

-- Read.hs
cpCheckAvailable :: IsCheckpoint c
                 => Set Core.TxIn -> c -> (Set Core.TxIn, Set Core.TxIn)
cpCheckAvailable ins c = Set.partition isAvailable ins
  where
    isAvailable :: Core.TxIn -> Bool
    isAvailable inp = inp `Map.member` cpAvailableUtxo c

```
[newPending/Foreign](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L89-L139)
/
[Pending.insert](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Pending.hs#L87-L88)
/
[cpCheckAvailable](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Read.hs#L46-L52)

where

| Spec     		| Code       			  |
| ------   		| --------- 			  |
| pending U {tx} | insertPending		  |
| **precondition:** ins `subset` dom (available) | cpCheckAvailable |

* NOTE: the precondition `cpCheckAvailable ` on `newPending` is defined in Fig 3 of the Wallet Spec.

### newForeign

This is a variation on `newPending` required for the Cardano implementation and is not discussed in the Wallet Spec.

`newForeign` allows for transfering funds from another wallet to the local wallet. The invariant is the inverse of `newPending`, in this case _none_ of the inputs must belong to the addressses of the receiving wallet.

Foreign transactions have seperate state from Pendings in the wallet checkpoint, but should otherwise be considered as Pending transactions in the Wallet Spec.

A special use case of `newForeign` is for redemptions, which are transactions that spend outputs not owned by local wallet.

## Section 6 - Rollback

### 6.1 Model

see [Introduction\The Cardano Wallet\Wallet State](#the-cardano-wallet)

### 6.4 Memory Requirements

Note that the only wallet operation that adds a new checkpoint is `applyBlock`. Cardano stores only the last `k` checkpoints, see `takeNewest k` in
[Spec.Update.applyBlock/Partial](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L148-L200)

### 6.6 Omitting checkpoints

Cardano ensures that empty checkpoints (for _all_ wallet accounts) are created for every new block the wallet is notified of. See [AcidState.applyBlock](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/AcidState.hs#L223)

See [rollback base case](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L216) for the Cardano implementation of a "useful optimisation opportunity: we can simply leave all the empty checkpoints as implicit".

### rollback (7/11)

```haskell
rollback :: NewestFirst StrictNonEmpty Checkpoint
         -> (NewestFirst StrictNonEmpty Checkpoint, Pending)
rollback (NewestFirst (c :| SL.Nil))        = (NewestFirst $ c :| SL.Nil, Pending.empty)
rollback (NewestFirst (c :| SL.Cons c' cs)) = (NewestFirst $ Checkpoint {
        _checkpointUtxo        = c' ^. checkpointUtxo
      , _checkpointUtxoBalance = c' ^. checkpointUtxoBalance
      , _checkpointBlockMeta   = c' ^. checkpointBlockMeta
      , _checkpointSlotId      = c' ^. checkpointSlotId
      , _checkpointPending     = Pending.union (c  ^. checkpointPending)
                                               (c' ^. checkpointPending)
      , _checkpointForeign     = Pending.union (c  ^. checkpointForeign)
                                               (c' ^. checkpointForeign)
      } :| cs
    , Pending.union
        ((c' ^. checkpointPending) Pending.\\ (c ^. checkpointPending))
        ((c' ^. checkpointForeign) Pending.\\ (c ^. checkpointForeign))
    )
```
[rollback](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L202-L230)

where

| Spec     		| Code       |
| ------   		| --------   |
| utxo'  	           | c' ^. checkpointUtxo           |
| pending U pending' | Pending.union (c ...) (c' ...) |
| blockMeta'         | c' ^. checkpointBlockMeta      |

### switch(ToFork)

* Spec Section 6.5 "Switching to a fork"

**Fig 7/11 - Wallet Spec**

```haskell
switchToFork :: SecurityParameter
             -> Int  -- ^ Number of blocks to rollback
             -> OldestFirst [] (SlotId, PrefilteredBlock) -- ^ Blocks to apply
             -> NewestFirst StrictNonEmpty Checkpoint
             -> (NewestFirst StrictNonEmpty Checkpoint, (Pending, Set Txp.TxId))
switchToFork k numRollbacks blocksToApply = \cps ->
    rollbacks Pending.empty numRollbacks cps
  where
    rollbacks :: Pending -- Accumulator: reintroduced pending transactions
              -> Int
              -> NewestFirst StrictNonEmpty Checkpoint
              -> (NewestFirst StrictNonEmpty Checkpoint, (Pending, Set Txp.TxId))
    rollbacks !accNew 0 cps =
        applyBlocks
          accNew
          Set.empty
          (getOldestFirst blocksToApply)
          cps
    rollbacks !accNew n cps =
        rollbacks (Pending.union accNew reintroduced) (n - 1) cps'
      where
        (cps', reintroduced) = rollback cps

    applyBlocks :: Pending -- Accumulator: reintroduced pending transactions
                -> Set Txp.TxId -- Accumulator: removed pending transactions
                -> [(SlotId, PrefilteredBlock)]
                -> NewestFirst StrictNonEmpty Checkpoint
                -> (NewestFirst StrictNonEmpty Checkpoint, (Pending, Set Txp.TxId))
    applyBlocks !accNew !accRem []               cps = (cps, (accNew, accRem))
    applyBlocks !accNew !accRem ((slotId, b):bs) cps =
        applyBlocks
          (Pending.delete removed accNew)
          (Set.union      removed accRem)
          bs
          cps'
      where
       (cps', removed) = applyBlock k slotId b cps

```
[switchToFork](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L239-L283)

### change

**Fig 7 - Wallet Spec**

```haskell
cpChange :: IsCheckpoint c => IxSet (Indexed HdAddress) -> c -> Core.Utxo
cpChange ours cp =
    Map.union
      (Pending.change ours' $ cp ^. cpPending)
      (Pending.change ours' $ cp ^. cpForeign)
  where
    ours' :: Core.Address -> Bool
    ours' addr = IxSet.size (IxSet.getEQ (V1 addr) ours) == 1
```
[cpChange](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Read.hs#L65-L75)

and

```haskell
txOuts :: Pending -> Core.Utxo
txOuts p = rawTxOuts p `Core.utxoRemoveInputs` txIns p

-- | Outputs in 'txOuts' that belong to the wallet
change :: (Core.Address -> Bool) -> Pending -> Core.Utxo
change p = Map.filter p' . txOuts
  where
    p' :: Core.TxOutAux -> Bool
    p' = p . Core.txOutAddress . Core.toaOut
```
[src](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Pending.hs#L144-L158)

## Section 9 - Tracking Metadata

### Section 9.2 Transaction History

#### Section 9.2.1 Static Information

```haskell
data TxMeta = TxMeta {
      _txMetaId         :: Txp.TxId
    , _txMetaAmount     :: Core.Coin
    , _txMetaInputs     :: NonEmpty (Core.Address, Core.Coin, Txp.TxId, Word32)
    , _txMetaOutputs    :: NonEmpty (Core.Address, Core.Coin)
    , _txMetaCreationAt :: Core.Timestamp
    , _txMetaIsLocal    :: Bool
    , _txMetaIsOutgoing :: Bool
    , _txMetaWalletId   :: Core.Address
    , _txMetaAccountIx  :: Word32
    }
```
[TxMeta](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/TxMeta/Types.hs#L71-L108)

#### Persisting transaction metadata

After adding a new transaction (`newPending`/`newForeign`) to the wallet state, `putTxMeta` records the transaction metadata.

Note: Cardano wallet state is persisted with an `AcidState` database and transaction metadata with `sqlite`.

```haskell
putTxMeta' :: Maybe TxMeta -> IO ()
putTxMeta' (Just meta) = putTxMeta (walletPassive ^. walletMeta) meta
putTxMeta' Nothing     = pure ()
```
[putTxMeta'](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Pending.hs#L110-L112)

#### Section 9.2.2 Information dependent on chain status

**DSL**

Note: the DSL uses the Cardano SlotId (since slots are not modeled in the DSL) and AddressMeta (since it simply consists of Bools)

```haskell
data BlockMeta' h = BlockMeta' {
      _blockMetaSlotId'      :: Map (h (DSL.Transaction h Addr)) SlotId

    , `AddressMeta`
      _blockMetaAddressMeta' :: Map Addr AddressMeta
    }
```
[BlockMeta'](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/test/unit/UTxO/Interpreter.hs#L535-L546)

**CARDANO**

```haskell
data BlockMeta = BlockMeta {
      _blockMetaSlotId      :: !(InDb (Map Txp.TxId Core.SlotId))
    , _blockMetaAddressMeta :: !(InDb (Map Core.Address AddressMeta))
    } deriving Eq

data AddressMeta = AddressMeta {
      _addressMetaIsUsed   :: Bool
    , _addressMetaIsChange :: Bool
    } deriving Eq

instance Semigroup AddressMeta where
  a <> b = mergeAddrMeta a b
    where
      mergeAddrMeta :: AddressMeta -> AddressMeta -> AddressMeta
      mergeAddrMeta (AddressMeta used change) (AddressMeta used' change')
          = AddressMeta (used || used') (change `xor` change')

instance Monoid AddressMeta where
  mempty  = AddressMeta False False
  mappend = (<>)

addressMeta :: Core.Address -> Lens' BlockMeta AddressMeta
addressMeta addr = blockMetaAddressMeta . fromDb . at addr . non mempty
```
[BlockMeta](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/BlockMeta.hs#L70-L76)
/
[AddressMeta SemiGroup/Monoid](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/BlockMeta.hs#L52-L61)
/
[addressMeta Lens](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/BlockMeta.hs#L88-L91)

Notes:

* the `AddressMeta` definition is `mempty` says that by default, a transaction is considered _not_ used or change
* the definition of `(<>)` on the `SemiGroup` instance of `AddressMeta` very literally expresses the formula at the top of p29 in the Wallet Spec

#### PartialCheckpoints

In the Wallet Spec, BlockMeta is a monoid, whereas in Cardano it is not since we need to distinguish between block metadata derived from the entire chain and metadata computed for a few blocks (`LocalBlockMeta`).

On the other hand, `AddressMeta` *is* a monoid in Cardano.

A `PartialCheckpoint` becomes relevant during wallet restoration, when we have incomplete context of the blockchain. A partial checkpoint differs from a regular Checkpoint in that it has "local" block metadata:

```haskell
data PartialCheckpoint = PartialCheckpoint {
      _pcheckpointUtxo        :: !(InDb Core.Utxo)
    , _pcheckpointUtxoBalance :: !(InDb Core.Coin)
    , _pcheckpointPending     :: !Pending
    , _pcheckpointBlockMeta   :: !LocalBlockMeta
    , _pcheckpointSlotId      :: !(InDb Core.SlotId)
    , _pcheckpointForeign     :: !Pending
    }
```
[PartialCheckpoint](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec.hs#L132-L147)

`LocalBlockMeta` is simply a type marker to indicate that block metadata may be incomplete.

During restoration we construct partial checkpoints with local block metadata. At the end of restoration we can restore to full Checkpoints (with complete block metadata):

```haskell
finishRestoration :: HdAccountWithinK -> HdAccountUpToDate
finishRestoration HdAccountWithinK{..} = HdAccountUpToDate{
      _hdUpToDateCheckpoints =
           map (toFullCheckpoint mostRecent) _hdWithinKCurrent
        <> _hdWithinKHistorical
    }
  where
    NewestFirst (mostRecent :| _) = _hdWithinKHistorical
```
[finishRestoration](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/HdWallet.hs#L379-L393)

Note: when applyBlock is called on a partial checkpoint in an account, we call the variant `applyBlockPartial` instead of `applyBlock`.

`Checkpoint` and `PartialCheckpoint` are unified by the typeclass

```haskell
class IsCheckpoint c where
    cpUtxo        :: Lens' c Core.Utxo
    cpUtxoBalance :: Lens' c Core.Coin
    cpPending     :: Lens' c Pending
    cpBlockMeta   :: Lens' c LocalBlockMeta
    cpSlotId      :: Lens' c Core.SlotId
    cpForeign     :: Lens' c Pending
```
[IsCheckpoint](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec.hs#L220-L255)

#### updateBlockMeta

(see `applyBlock` in Section 5 - Prefiltering)

Note: for a `PrefilteredBlock` all intputs and outputs are restricted to "ours".

```haskell
updateBlockMeta :: PrefilteredBlock -> BlockMeta -> BlockMeta
updateBlockMeta = flip appendBlockMeta . pfbMeta

appendBlockMeta :: BlockMeta -> LocalBlockMeta -> BlockMeta
appendBlockMeta cur (LocalBlockMeta new) = BlockMeta {
        _blockMetaSlotId      = combineUsing (liftA2 Map.union)
                                  _blockMetaSlotId
      , _blockMetaAddressMeta = combineUsing (liftA2 (Map.unionWith (<>)))
                                  _blockMetaAddressMeta
      }
  where
    combineUsing :: (a -> a -> a) -> (BlockMeta -> a) -> a
    combineUsing op f = f cur `op` f new
```
[updateBlockMeta](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L289-L293)
/
[appendBlockMeta](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/BlockMeta.hs#L115-L139)

where

**Fig 11 - Wallet Spec**

| Spec    | Code       					|
| ------- | ---------- 					|
| blockMeta U+ blockMeta' | appendBlockMeta |
| "take the union of block numbers" | _blockMetaSlotId = combineUsing (liftA2 Map.union) |
| (u,c) U+ (u',c') | _blockMetaAddressMeta = combineUsing (liftA2 (Map.unionWith (<>))) |

Block and Address metadata are constructed during the prefiltering stage and express the definitions in the Wallet Spec 9.2.2:

```haskell
mkAddressMeta :: NE.NonEmpty AddressSummary -> AddressMeta
mkAddressMeta addrs
    = AddressMeta isUsed isChange
    where
        occurs = NE.length addrs


        -- An address is considered "used" if
        -- (1) it is "our" address: we are only dealing with prefiltered transactions
        --     here and can at this stage assume that the address is indeed "ours".
        -- (2) the transaction is confirmed: we are dealing here with transactions that
        --     appear in a block and can assume that they are confirmed.
        isUsed = True


        -- An address is considered "change" if
        -- (1) it is "our" address: as with `isUsed` above, we can assume the address is "ours"
        -- (2) the address occurs in exactly one transaction in this block
        -- (3) for the (single) transaction in which this address appears, the
        --     outputs must not all be to "our" addresses (the transaction must have
        --     an output to at least one address that is not "ours")
        -- (4) all the inputs of the transaction in which this address appears
        --     must be "ours"
        isChange = (occurs == 1)                    -- (2)
                    && addrSummaryOnlyOurInps       -- (3)
                    && not addrSummaryOnlyOurOuts   -- (4)
            where AddressSummary{..} = NE.head addrs
```

[mkAddressMeta](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/PrefilterTx.hs#L304-L345)

#### Section 9.2.3 Transaction Status

```haskell
data TransactionStatus
    = Applying
    | InNewestBlocks
    | Persisted
    | WontApply
    | Creating
    deriving (Eq, Show, Ord)
```

[TransactionStatus](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/API/V1/Types.hs#L1878-L1885)

In Cardano, Transaction Status is computed as part of `getTransactions` in the [Wallet layer Kernel](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/WalletLayer/Kernel/Transactions.hs#L116-L158).

The Transaction status is derived in

```haskell
buildDynamicTxMeta :: HD.AssuranceLevel -> SlotCount -> Maybe SlotId -> SlotId -> Bool -> (V1.TransactionStatus, Word64)
buildDynamicTxMeta assuranceLevel slotCount mSlot currentSlot isPending = case isPending of
    True  -> (V1.Applying, 0)
    False ->
        case mSlot of
        Nothing     -> (V1.WontApply, 0)
        Just confirmedIn ->
            let currentSlot'  = flattenSlotIdExplicit slotCount currentSlot
                confirmedIn'  = flattenSlotIdExplicit slotCount confirmedIn
                confirmations = currentSlot' - confirmedIn'
            in case (confirmations < getBlockCount (HD.assuredBlockDepth assuranceLevel)) of
               True  -> (V1.InNewestBlocks, confirmations)
               False -> (V1.Persisted, confirmations)
```

* `Applying` if the transaction is Pending
* `WontApply` if there is no available slotId
* The Wallet Spec uses _k_ to distinguish between `InNewestBlocks` and `Persisted`. For the new wallet we have maintained compatibility with the old by using the `getBlockCount` (instead of _k_) specified by the assurance level of the wallet.

If we had used _k_, users would experience the new wallet as "taking much longer to confirm my transactions". However, these numbers below (9 and 5) are significantly lower than _k_ (2160) and will report a status as `Persisted` well within the range that the transaction may still be rolled back:

```haskell
assuredBlockDepth :: AssuranceLevel -> Core.BlockCount
assuredBlockDepth AssuranceLevelNormal = 9
assuredBlockDepth AssuranceLevelStrict = 15
```
[assuredBlockDepth](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/HdWallet.hs#L153-L159)

**IsLocal, IsOutgoing**

The Wallet Spec 9.2.1 is implemented by `_txMetaIsLocal` and `_txMetaIsOutgoing` below:

```haskell
-- | This is used when apply block is called, during prefiltering, so related inputs
-- and outputs to the HDAccount are known to the caller.
-- @spentInputsCoins@ is the coins from input addresses of the account. They reduce the balance.
-- @gainedOutputsCoins@ is the coins from output addresses of the account. They increase the balance.
-- @allOurs@ indictes if all inputs and outputs addresses belong to the account.
resolvedToTxMeta :: ResolvedTx -> Coin -> Coin -> Bool -> HD.HdAccountId -> TxMeta
resolvedToTxMeta ResolvedTx{..} spentInputsCoins gainedOutputsCoins allOurs accountId =
  fromMaybe (error "Invalid ResolvedTx") mbMeta
  where
    mbMeta = do
      inps <- NE.nonEmpty $ mapMaybe toInpQuad $ NE.toList (_fromDb _rtxInputs)
      outs <- fromUtxo $ _fromDb _rtxOutputs
      let (txId, timestamp) = _fromDb _rtxMeta
      return TxMeta {
          _txMetaId = txId
        , _txMetaAmount = absCoin inCoin outCoin
        , _txMetaInputs = inps
        , _txMetaOutputs = outs
        , _txMetaCreationAt = timestamp
        , _txMetaIsLocal = allOurs
        , _txMetaIsOutgoing = gainedOutputsCoins < spentInputsCoins -- it's outgoing if gained is less than spent.
        , _txMetaWalletId = _fromDb $ HD.getHdRootId (accountId ^. HD.hdAccountIdParent)
        , _txMetaAccountIx = HD.getHdAccountIx $ accountId ^. HD.hdAccountIdIx
      }
```

Note: the above code was added shortly after this document was pinned to a git hash (hence no link).

**Fig 13 - Wallet Spec**

```haskell
addPending :: HdAccountId -> Pending -> WalletSubmission -> WalletSubmission

remPending :: Map HdAccountId (Set Txp.TxId)
           -> WalletSubmission
           -> WalletSubmission

tick :: WalletSubmission
     -> (Cancelled, [Txp.TxAux], WalletSubmission)

```
[addPending](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Submission.hs#L295)
/
[remPending](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Submission.hs#L306-L308)
/
[tick](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Submission.hs#L321-L331)

## Section 10 - Transaction Submission

### 10.1 Interface

#### Adding and removing pending transactions
* "call `addPending` on `newPending` and (possibly) on `rollback`"
* "call `remPending` on `applyBlock` and `cancel`"

After adding a new transaction (`newPending`/`newForeign`) to the wallet state, we notify the submission layer of the new transaction:

```haskell
submitTx :: IO ()
submitTx = modifyMVar_ (walletPassive ^. walletSubmission) $
                    return . addPending accountId (Pending.singleton tx)
```
[submitTx](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Pending.hs#L114-L116)

In `applyBlock` we remove relevant pending transactions:

```haskell
-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO ()
applyBlock pw@PassiveWallet{..} b = do
    k <- Node.getSecurityParameter _walletNode
    ((slotId, blocksByAccount), metas) <- prefilterBlock' pw b
    -- apply block to all Accounts in all Wallets
    confirmed <- update' _wallets $ ApplyBlock k (InDb slotId) blocksByAccount
    modifyMVar_ _walletSubmission $ return . Submission.remPending confirmed
    mapM_ (putTxMeta _walletMeta) metas
```
[BListener.applyBlock](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/BListener.hs#L54-L64)

In `switchToFork` we _add_ / _remove_ the relevant pending transactions to/from the submission layer (which corresponds with the Wallet Spec on possibly adding pendings during `rollback` and removing pendings during `applyBlock`):

```haskell
switchToFork :: PassiveWallet
             -> Int             -- ^ Number of blocks to roll back
             -> [ResolvedBlock] -- ^ Blocks in the new fork
             -> IO (Either RollbackDuringRestoration ())
switchToFork pw@PassiveWallet{..} n bs = do
    k <- Node.getSecurityParameter _walletNode
    blocksAndMeta <- mapM (prefilterBlock' pw) bs
    let (blockssByAccount, metas) = unzip blocksAndMeta
    res <- update' _wallets $ SwitchToFork k n blockssByAccount
    case res of
      Left  err     -> return $ Left err
      Right changes -> do mapM_ (putTxMeta _walletMeta) $ concat metas
                          modifyMVar_ _walletSubmission $
                            return . Submission.addPendings (fst <$> changes)
                          modifyMVar_ _walletSubmission $
                            return . Submission.remPending (snd <$> changes)
                          return $ Right ()
```
[switchToFork](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/BListener.hs#L74-L94)

#### Tick function

* "must be a thread that periodically calls tick, to give the submission layer a chance to resubmit transactions that haven’t made it into the blockchain yet. The set of transactions returned by tick ... the wallet should remove such transactions from its pending set"

The submission layer resource is managed in `bracketActiveWallet` and is initialised with `tickFunction`, which calls `tick` and cancels any pending transactions returned:

```haskell
tickFunction :: MVar WalletSubmission -> IO ()
tickFunction submissionLayer = do
    (cancelled, toSend) <-
        modifyMVar submissionLayer $ \layer -> do
            let (e, s, state') = tick layer
            return (state', (e,s))
    unless (Map.null cancelled) $
        cancelPending walletPassive cancelled
    sendTransactions toSend
```
[tickFunction](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel.hs#L152-L160)

#### cancelPending

```haskell
-- | Cancel a pending transaction
--
-- NOTE: This gets called in response to events /from/ the wallet submission
-- layer, so we shouldn't be notifying the submission in return here.
--
-- This removes the transaction from either pending or foreign.
cancelPending :: forall c. IsCheckpoint c
              => Set Txp.TxId
              -> NewestFirst StrictNonEmpty c -> NewestFirst StrictNonEmpty c
cancelPending txids = map (cpPending %~ Pending.delete txids)
```
[cancelPending](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/DB/Spec/Update.hs#L141-L146)

### 10.2 Implementation

* The `Schedule` time slots are modeled as a collection of `ScheduleEvents` per `slotId`

```haskell
data Schedule = Schedule {
      _ssScheduled     :: IntMap ScheduleEvents
    , _ssUnsentNursery :: [ScheduleSend]
    }

-- | A type representing an item (in this context, a transaction) scheduled
-- to be regularly sent in a given slot (computed by a given 'RetryPolicy').
data ScheduleSend = ScheduleSend HdAccountId Txp.TxId Txp.TxAux SubmissionCount deriving Eq

-- | A type representing an item (in this context, a transaction @ID@) which
-- needs to be checked against the blockchain for inclusion. In other terms,
-- we need to confirm that indeed the transaction identified by the given 'TxId' has
-- been adopted, i.e. it's not in the local pending set anymore.
data ScheduleEvictIfNotConfirmed = ScheduleEvictIfNotConfirmed HdAccountId Txp.TxId deriving Eq

-- | All the events we can schedule for a given 'Slot', partitioned into
-- 'ScheduleSend' and 'ScheduleEvictIfNotConfirmed'.
data ScheduleEvents = ScheduleEvents {
      _seToSend    :: [ScheduleSend]
    -- ^ A list of transactions which we need to send.
    , _seToConfirm :: [ScheduleEvictIfNotConfirmed]
    -- ^ A list of transactions which we need to check if they have been
    -- confirmed (i.e. adopted) by the blockchain.
    }

```

* "When the submission layer is notified of new pending transactions, it adds those to its pending set and schedules them to be submitted in the next slot, recording an initial submission count of 0"

```haskell
-- | Schedule the full list of pending transactions.
-- The transactions will be scheduled immediately in the next 'Slot'.
schedulePending :: HdAccountId
                -> Pending
                -> WalletSubmission
                -> WalletSubmission
schedulePending accId pending ws =
    let currentSlot = ws ^. wsState . wssCurrentSlot
    in addToSchedule ws (mapSlot succ currentSlot) toSend mempty
    where
        toEntry :: (Txp.TxId, Txp.TxAux) -> ScheduleSend
        toEntry (txId, txAux) = ScheduleSend accId txId txAux (SubmissionCount 0)

        toSend :: [ScheduleSend]
        toSend = map toEntry (Pending.toList pending)
```

* "The submission layer is parameterised over a ‘resubmission function’ ρ"
* "If desired, the submission count can be used to implement exponential back-off"

```haskell

-- see initPassiveWallet:

submission <- newMVar (newWalletSubmission rho)
...
rho = defaultResubmitFunction (exponentialBackoff 255 1.25)
```
[initPassiveWallet](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel.hs#L105)

### 10.3 Persistence

* "The state of the submission layer does not need to be persisted. If the wallet is shutdown for some period of time, the submission layer can simply be re-initialised from the state of the wallet, starting the submission process afresh for any transactions that the wallet still reports as pending."

```haskell

-- see initPassiveWallet...

initSubmission :: PassiveWallet -> IO ()
initSubmission pw  = do
    pendings <- pendingByAccount <$> getWalletSnapshot pw
    modifyMVar_ (_walletSubmission pw) $
	    return . addPendings pendings
```
Note: the above code was added shortly after this document was pinned to a git hash (hence no link).

### 10.4 Transactions with TTL

Cardano does not implement transaction TTL yet and instead relies on submission count and a maximum retry limit.

**Fig 14 - Wallet Spec**

```haskell
-- TYPES

data Schedule = Schedule {
      _ssScheduled     :: IntMap ScheduleEvents
    , _ssUnsentNursery :: [ScheduleSend]
    }

-- RESUMBISSION FUNCTION (rho)

type ResubmissionFunction =  Slot
                          -> [ScheduleSend]
                          -> Schedule
                          -> (Schedule, [Txp.TxAux])


-- STATE

data WalletSubmissionState = WalletSubmissionState {
      _wssPendingMap  ::  M.Map HdAccountId Pending
    , _wssSchedule    ::  Schedule
    , _wssCurrentSlot :: !Slot
    }

-- ATOMIC UPDATES

addPending :: HdAccountId -> Pending -> WalletSubmission -> WalletSubmission
addPending accId newPending ws =
    let ws' = ws & over (pendingByAccId accId)
                        (Pending.union newPending)
    in schedulePending accId newPending ws'

remPending :: Map HdAccountId (Set Txp.TxId)
           -> WalletSubmission
           -> WalletSubmission
remPending pendingMap ws =
    M.foldlWithKey' (\acc accId pendingSet ->
                        remPendingById accId pendingSet acc
                    ) ws pendingMap

tick :: WalletSubmission
     -> (Cancelled, [Txp.TxAux], WalletSubmission)

```

[Schedule](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Submission.hs#L117-L138)
/
[ResubmissionFunction](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Submission.hs#L227-L239)
/
[WalletSubmissionState](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Submission.hs#L96-L108)
/
[addPending](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Submission.hs#L295)
[remPending](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Submission.hs#L306-L308)
/
[tick](https://github.com/input-output-hk/cardano-sl/blob/6659d8501c727714a7861ad2e527a337e0a11b86/wallet-new/src/Cardano/Wallet/Kernel/Submission.hs#L321-L331)
