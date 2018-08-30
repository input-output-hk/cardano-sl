{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}

-- | UPDATE operations on the wallet-spec state
module Cardano.Wallet.Kernel.DB.Spec.Update (
    -- * Errors
    NewPendingFailed(..)
  , NewForeignFailed(..)
  , ApplyBlockFailed(..)
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

import           Universum hiding ((:|))

import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJsonIndent)
import           Test.QuickCheck (Arbitrary (..), elements)

import qualified Pos.Chain.Block as Core
import           Pos.Chain.Txp (Utxo)
import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core
import           Pos.Core.Chrono (NewestFirst (..), OldestFirst (..))

import           Cardano.Wallet.Kernel.DB.BlockContext
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
import qualified Cardano.Wallet.Kernel.Util.StrictList as SL
import           Cardano.Wallet.Kernel.Util.StrictNonEmpty (StrictNonEmpty (..))
import qualified Cardano.Wallet.Kernel.Util.StrictNonEmpty as SNE
import           UTxO.Util (liftNewestFirst)

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

-- | Errors thrown by 'applyBlock'
data ApplyBlockFailed =
    -- | The block we're trying to apply does not fit onto the previous
    --
    -- This indicates that the wallet has fallen behind the node (for example,
    -- when the node informs the wallet of a block but the wallet gets
    -- shut down before it gets a chance to process it).
    --
    -- We record  the context of the block we're trying to apply and the
    -- context of the most recent checkpoint.
    ApplyBlockNotSuccessor BlockContext (Maybe BlockContext)

deriveSafeCopy 1 'base ''ApplyBlockFailed

instance Buildable ApplyBlockFailed where
    build (ApplyBlockNotSuccessor context checkpoint) = bprint
        ("ApplyBlockNotSuccessor "
        % "{ context:    " % build
        % ", checkpoint: " % build
        % "}"
        )
        context
        checkpoint

instance Arbitrary ApplyBlockFailed where
   arbitrary = elements []

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
           -> Update' NewPendingFailed (Checkpoints c) ()
newPending (InDb tx) = do
    checkpoints <- get
    let (_available, unavailable) =
           cpCheckAvailable (Core.txIns tx) (checkpoints ^. currentCheckpoint)
    if Set.null unavailable
      then put $ insertPending checkpoints
      else throwError $ NewPendingInputsUnavailable (InDb unavailable)
  where
    insertPending :: Checkpoints c -> Checkpoints c
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
           -> Update' NewForeignFailed (Checkpoints c) ()
newForeign (InDb tx) = do
    checkpoints <- get
    let (available, _unavailable) =
           cpCheckAvailable (Core.txIns tx) (checkpoints ^. currentCheckpoint)
    if Set.null available
      then put $ insertForeign checkpoints
      else throwError $ NewForeignInputsAvailable (InDb available)
  where
    insertForeign :: Checkpoints c -> Checkpoints c
    insertForeign = currentForeign %~ Pending.insert tx

-- | Cancel the input set of cancelled transactions from @all@ the 'Checkpoints'
-- of an 'Account'.
cancelPending :: forall c. IsCheckpoint c
              => Set Txp.TxId
              -> Checkpoints c -> Checkpoints c
cancelPending txids = liftCheckpoints $ map (cpPending %~ Pending.delete txids)

-- | Apply the prefiltered block to the specified wallet
--
-- Additionally returns the set of transactions removed from pending.
applyBlock :: SecurityParameter
           -> PrefilteredBlock
           -> Update' ApplyBlockFailed
                      (Checkpoints Checkpoint)
                      (Set Txp.TxId)
applyBlock (SecurityParameter k) pb = do
    checkpoints@(Checkpoints ls)  <- get
    let current           = checkpoints ^. currentCheckpoint
        utxo              = current ^. checkpointUtxo        . fromDb
        balance           = current ^. checkpointUtxoBalance . fromDb
        (utxo', balance') = updateUtxo      pb (utxo, balance)
        (pending', rem1)  = updatePending   pb (current ^. checkpointPending)
        blockMeta'        = updateBlockMeta pb (current ^. checkpointBlockMeta)
        (foreign', rem2)  = updatePending   pb (current ^. checkpointForeign)
    if (pfbContext pb) `blockContextSucceeds` (current ^. checkpointContext) then do
      put $ Checkpoints . takeNewest k . NewestFirst $ Checkpoint {
          _checkpointUtxo        = InDb utxo'
        , _checkpointUtxoBalance = InDb balance'
        , _checkpointPending     = pending'
        , _checkpointBlockMeta   = blockMeta'
        , _checkpointForeign     = foreign'
        , _checkpointContext     = Just $ pfbContext pb
        } SNE.<| getNewestFirst ls
      return $ Set.unions [rem1, rem2]
    else
      throwError $ ApplyBlockNotSuccessor
                     (pfbContext pb)
                     (current ^. checkpointContext)

-- | Like 'applyBlock', but to a list of partial checkpoints instead
--
-- NOTE: Unlike 'applyBlock', we do /NOT/ throw away partial checkpoints. If
-- we did, it might be impossible for the historical checkpoints to ever
-- catch up with the current ones.
applyBlockPartial :: PrefilteredBlock
                  -> Update' ApplyBlockFailed
                             (Checkpoints PartialCheckpoint)
                             (Set Txp.TxId)
applyBlockPartial pb = do
    checkpoints@(Checkpoints ls)  <- get
    let current           = checkpoints ^. currentCheckpoint
        utxo              = current ^. pcheckpointUtxo        . fromDb
        balance           = current ^. pcheckpointUtxoBalance . fromDb
        (utxo', balance') = updateUtxo           pb (utxo, balance)
        (pending', rem1)  = updatePending        pb (current ^. pcheckpointPending)
        blockMeta'        = updateLocalBlockMeta pb (current ^. pcheckpointBlockMeta)
        (foreign', rem2)  = updatePending        pb (current ^. pcheckpointForeign)
    if (pfbContext pb) `blockContextSucceeds` (current ^. cpContext) then do
      put $ Checkpoints $ NewestFirst $ PartialCheckpoint {
          _pcheckpointUtxo        = InDb utxo'
        , _pcheckpointUtxoBalance = InDb balance'
        , _pcheckpointPending     = pending'
        , _pcheckpointBlockMeta   = blockMeta'
        , _pcheckpointForeign     = foreign'
        , _pcheckpointContext     = pfbContext pb
        } SNE.<| getNewestFirst ls
      return $ Set.unions [rem1, rem2]
    else
      throwError $ ApplyBlockNotSuccessor
                     (pfbContext pb)
                     (current ^. cpContext)

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
rollback :: Update' e (Checkpoints Checkpoint) Pending
rollback = state $ \case
    Checkpoints (NewestFirst (c :| SL.Nil)) -> (Pending.empty,
                                                Checkpoints . NewestFirst $ c :| SL.Nil)
    Checkpoints (NewestFirst (c :| SL.Cons c' cs)) ->
        (Pending.union
            ((c' ^. cpPending) Pending.\\ (c ^. cpPending))
            ((c' ^. cpForeign) Pending.\\ (c ^. cpForeign)),
         Checkpoints . NewestFirst $ (c' & cpPending %~ Pending.union (c ^. cpPending)
                                         & cpForeign %~ Pending.union (c ^. cpForeign)) :| cs)

-- | Observable rollback, used in testing only
--
-- See 'switchToFork' for production use.
observableRollbackUseInTestsOnly :: Update' e (Checkpoints Checkpoint) Pending
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
             -> Maybe Core.HeaderHash  -- ^ Roll back until we meet this block.
             -> OldestFirst [] PrefilteredBlock -- ^ Blocks to apply
             -> Update' ApplyBlockFailed
                        (Checkpoints Checkpoint)
                        (Pending, Set Txp.TxId)
switchToFork k oldest blocksToApply = do
    reintroduced <- rollbacks Pending.empty
    applyBlocks reintroduced Set.empty (getOldestFirst blocksToApply)
  where
    rollbacks :: Pending -- Accumulator: reintroduced pending transactions
              -> Update' e
                         (Checkpoints Checkpoint)
                         Pending
    rollbacks !accNew = do
        curCtx <- use currentContext
        reintroduced <- rollback
        let acc = Pending.union accNew reintroduced
            prev = join (curCtx <&> _bcPrevMain) <&> _fromDb

        case (prev == oldest, prev) of
            (True, _)        -> return acc    -- We rolled back everything we needed to.
            (False, Nothing) -> return acc    -- The checkpoints began after the fork point.
            (False, Just _)  -> rollbacks acc -- Keep going

    applyBlocks :: Pending -- Accumulator: reintroduced pending transactions
                -> Set Txp.TxId -- Accumulator: removed pending transactions
                -> [PrefilteredBlock]
                -> Update' ApplyBlockFailed
                           (Checkpoints Checkpoint)
                           (Pending, Set Txp.TxId)
    applyBlocks !accNew !accRem []     = return (accNew, accRem)
    applyBlocks !accNew !accRem (b:bs) = do
        removed <- applyBlock k b
        applyBlocks (Pending.delete removed accNew)
                    (Set.union      removed accRem)
                    bs

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
    balance'  = Core.unsafeIntegerToCoin $
                    Core.coinToInteger balance
                  + Core.utxoBalance pfbOutputs
                  - Core.utxoBalance utxoMin

-- | Update the pending transactions with the given prefiltered block
--
-- Returns the set of transactions that got removed from the pending set.
updatePending :: PrefilteredBlock -> Pending -> (Pending, Set Txp.TxId)
updatePending PrefilteredBlock{..} = Pending.removeInputs pfbInputs

takeNewest :: Int -> NewestFirst StrictNonEmpty a -> NewestFirst StrictNonEmpty a
takeNewest = liftNewestFirst . SNE.take
