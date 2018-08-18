{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

-- | UPDATE operations on the wallet-spec state
module Cardano.Wallet.Kernel.DB.Spec.Update (
    -- * Errors
    NewPendingFailed(..)
  , NewForeignFailed(..)
    -- * Updates
  , newPending
  , newForeign
  , cancelPending
  , applyBlock
  , applyBlockPartial
  , switchToFork
    -- * Testing
  , observableRollbackUseInTestsOnly
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set
import           Formatting (bprint, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJsonIndent)
import           Test.QuickCheck (Arbitrary (..))

import           Pos.Chain.Txp (Utxo)
import qualified Pos.Core as Core
import           Pos.Core.Chrono (NewestFirst (..), OldestFirst (..))
import           Pos.Core.Slotting (SlotId)
import qualified Pos.Core.Txp as Txp

import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.Spec.Read
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.NodeStateAdaptor (SecurityParameter (..))
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..))
import qualified Cardano.Wallet.Kernel.Util.Core as Core

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'newPending'
data NewPendingFailed =
    -- | Some inputs are not in the wallet utxo
    NewPendingInputsUnavailable (InDb (Set Txp.TxIn))

deriveSafeCopy 1 'base ''NewPendingFailed

instance Buildable NewPendingFailed where
    build (NewPendingInputsUnavailable (InDb inputs)) =
        bprint ("NewPendingInputsUnavailable { inputs = " % listJsonIndent 4 % " }") (Set.toList inputs)

-- NOTE(adn) Short-circuiting the rabbit-hole with this instance by generating
-- an empty set, thus avoiding the extra dependency on @cardano-sl-core-test@.
instance Arbitrary NewPendingFailed where
    arbitrary = pure . NewPendingInputsUnavailable . InDb $ mempty

data NewForeignFailed =
    -- | Foreign transactions are not allowed spend from this wallet
    NewForeignInputsAvailable (InDb (Set Txp.TxIn))

deriveSafeCopy 1 'base ''NewForeignFailed

instance Buildable NewForeignFailed where
    build (NewForeignInputsAvailable (InDb inputs)) =
        bprint ("NewForeignInputsAvailable { inputs = " % listJsonIndent 4 % " }") (Set.toList inputs)

-- TODO: See comments for the 'Arbitrary' instance for 'NewPendingFailed'
instance Arbitrary NewForeignFailed where
    arbitrary = pure . NewForeignInputsAvailable . InDb $ mempty

{-------------------------------------------------------------------------------
  Wallet spec mandated updates
-------------------------------------------------------------------------------}

-- | Insert new pending transaction into the specified wallet
--
-- NOTE: Transactions to be inserted must be fully constructed and signed; we do
-- not offer input selection at this layer. Instead, callers must get a snapshot
-- of the database, construct a transaction asynchronously, and then finally
-- submit the transaction. It is of course possible that the state of the
-- database has changed at this point, possibly making the generated transaction
-- invalid; 'newPending' therefore returns whether or not the transaction could
-- be inserted. If this fails, the process must be started again. This is
-- important for a number of reasons:
--
-- * Input selection may be an expensive computation, and we don't want to
--   lock the database while input selection is ongoing.
-- * Transactions may be signed off-site (on a different machine or on a
--   a specialized hardware device).
-- * We do not actually have access to the key storage inside the DB layer
--   (and do not store private keys) so we cannot actually sign transactions.
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

-- | Insert new foreign transaction
--
-- For foreign transactions we expect /none/ of the inputs to belong to the
-- wallet. We may wish to relax this requirement later (or indeed get rid of
-- the difference between these two pending sets altogether), but for now this
-- strict separation makes it easier to see what's going on and limits the
-- impact this has on the reasoning in the wallet spec.
newForeign :: forall c. IsCheckpoint c
           => InDb Txp.TxAux
           -> Update' (NewestFirst NonEmpty c) NewForeignFailed ()
newForeign (InDb tx) = do
    checkpoints <- get
    let (available, _unavailable) =
           cpCheckAvailable (Core.txIns tx) (checkpoints ^. currentCheckpoint)
    if Set.null available
      then put $ insertForeign checkpoints
      else throwError $ NewForeignInputsAvailable (InDb available)
  where
    insertForeign :: NewestFirst NonEmpty c -> NewestFirst NonEmpty c
    insertForeign = currentForeign %~ Pending.insert tx

-- | Cancel the input set of cancelled transactions from @all@ the 'Checkpoints'
-- of an 'Account'.
cancelPending :: forall c. IsCheckpoint c
              => Set Txp.TxId
              -> NewestFirst NonEmpty c -> NewestFirst NonEmpty c
cancelPending txids = map (cpPending %~ Pending.delete txids)

-- | Apply the prefiltered block to the specified wallet
--
-- Additionally returns the set of transactions removed from pending.
applyBlock :: SecurityParameter
           -> SlotId
           -> PrefilteredBlock
           -> NewestFirst NonEmpty Checkpoint
           -> (NewestFirst NonEmpty Checkpoint, Set Txp.TxId)
applyBlock (SecurityParameter k) slotId pb checkpoints = (
      takeNewest k $ NewestFirst $ Checkpoint {
          _checkpointUtxo        = InDb utxo'
        , _checkpointUtxoBalance = InDb balance'
        , _checkpointPending     = pending'
        , _checkpointBlockMeta   = blockMeta'
        , _checkpointSlotId      = InDb slotId
        , _checkpointForeign     = foreign'
        } NE.<| getNewestFirst checkpoints
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

-- | Like 'applyBlock', but to a list of partial checkpoints instead
applyBlockPartial :: SecurityParameter
                  -> SlotId
                  -> PrefilteredBlock
                  -> NewestFirst NonEmpty PartialCheckpoint
                  -> (NewestFirst NonEmpty PartialCheckpoint, Set Txp.TxId)
applyBlockPartial (SecurityParameter k) slotId pb checkpoints = (
      takeNewest k $ NewestFirst $ PartialCheckpoint {
          _pcheckpointUtxo        = InDb utxo'
        , _pcheckpointUtxoBalance = InDb balance'
        , _pcheckpointPending     = pending'
        , _pcheckpointBlockMeta   = blockMeta'
        , _pcheckpointSlotId      = InDb slotId
        , _pcheckpointForeign     = foreign'
        } NE.<| getNewestFirst checkpoints
    , Set.unions [rem1, rem2]
    )
  where
    current           = checkpoints ^. currentCheckpoint
    utxo              = current ^. pcheckpointUtxo        . fromDb
    balance           = current ^. pcheckpointUtxoBalance . fromDb
    (utxo', balance') = updateUtxo           pb (utxo, balance)
    (pending', rem1)  = updatePending        pb (current ^. pcheckpointPending)
    blockMeta'        = updateLocalBlockMeta pb (current ^. pcheckpointBlockMeta)
    (foreign', rem2)  = updatePending        pb (current ^. pcheckpointForeign)

-- | Rollback
--
-- For the base case, see section "Rollback -- Omitting checkpoints" in the
-- formal specification.
--
-- NOTE: Rollback is currently only supported for wallets that are fully up
-- to date. Hence, we only support full checkpoints here.
--
-- Additionally returns the set of pending transactions that got reintroduced,
-- so that the submission layer can start sending those out again.
--
-- This is an internal function only, and not exported. See 'switchToFork'.
rollback :: NewestFirst NonEmpty Checkpoint
         -> (NewestFirst NonEmpty Checkpoint, Pending)
rollback (NewestFirst (c :| []))      = (NewestFirst $ c :| [], Pending.empty)
rollback (NewestFirst (c :| c' : cs)) = (NewestFirst $ Checkpoint {
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

-- | Observable rollback, used in testing only
--
-- See 'switchToFork' for production use.
observableRollbackUseInTestsOnly :: NewestFirst NonEmpty Checkpoint
                                 -> (NewestFirst NonEmpty Checkpoint, Pending)
observableRollbackUseInTestsOnly = rollback

-- | Switch to a fork
--
-- Since rollback is only supported on wallets that are up to date wrt to
-- the underlying node, the same goes for 'switchToFork'.
--
-- Additionally returns the set of transactions that got introduced reintroduced
-- (to the rollback) and the transactions that got removed from pending
-- (since they are new confirmed).
switchToFork :: SecurityParameter
             -> Int  -- ^ Number of blocks to rollback
             -> OldestFirst [] (SlotId, PrefilteredBlock) -- ^ Blocks to apply
             -> NewestFirst NonEmpty Checkpoint
             -> (NewestFirst NonEmpty Checkpoint, (Pending, Set Txp.TxId))
switchToFork k numRollbacks blocksToApply = \cps ->
    rollbacks Pending.empty numRollbacks cps
  where
    rollbacks :: Pending -- Accumulator: reintroduced pending transactions
              -> Int
              -> NewestFirst NonEmpty Checkpoint
              -> (NewestFirst NonEmpty Checkpoint, (Pending, Set Txp.TxId))
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
                -> NewestFirst NonEmpty Checkpoint
                -> (NewestFirst NonEmpty Checkpoint, (Pending, Set Txp.TxId))
    applyBlocks !accNew !accRem []               cps = (cps, (accNew, accRem))
    applyBlocks !accNew !accRem ((slotId, b):bs) cps =
        applyBlocks
          (Pending.delete removed accNew)
          (Set.union      removed accRem)
          bs
          cps'
      where
       (cps', removed) = applyBlock k slotId b cps

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

updateBlockMeta :: PrefilteredBlock -> BlockMeta -> BlockMeta
updateBlockMeta = flip appendBlockMeta . pfbMeta

updateLocalBlockMeta :: PrefilteredBlock -> LocalBlockMeta -> LocalBlockMeta
updateLocalBlockMeta = flip appendLocalBlockMeta . pfbMeta

-- | Update (utxo,balance) with the given prefiltered block
updateUtxo :: PrefilteredBlock -> (Utxo, Core.Coin) -> (Utxo, Core.Coin)
updateUtxo PrefilteredBlock{..} (utxo, balance) =
    (utxo', balance')
  where
    -- See wallet spec figure 6 (Wallet with prefiltering):
    --
    -- * pfbOutputs corresponds to what the spec calls utxo^+ / txouts_b
    -- * pfbInputs  corresponds to what the spec calls txins_b
    utxoUnion = Map.union utxo pfbOutputs
    utxoMin   = utxoUnion `Core.utxoRestrictToInputs` pfbInputs
    utxo'     = utxoUnion `Core.utxoRemoveInputs`     pfbInputs
    balance'  = fromMaybe (error "updateUtxo: out-of-range impossible") $ do
                  withNew <- Core.addCoin balance (Core.utxoBalance pfbOutputs)
                  Core.subCoin withNew (Core.utxoBalance utxoMin)

-- | Update the pending transactions with the given prefiltered block
--
-- Returns the set of transactions that got removed from the pending set.
updatePending :: PrefilteredBlock -> Pending -> (Pending, Set Txp.TxId)
updatePending PrefilteredBlock{..} = Pending.removeInputs pfbInputs

takeNewest :: Int -> NewestFirst NonEmpty a -> NewestFirst NonEmpty a
takeNewest k (NewestFirst (a :| as)) = NewestFirst (a :| take (k - 1) as)
