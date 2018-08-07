-- | UPDATE operations on the wallet-spec state
module Cardano.Wallet.Kernel.DB.Spec.Update (
    -- * Errors
  NewPendingFailed(..)
    -- * Updates
  , newPending
  , cancelPending
  , applyBlock
  , switchToFork
    -- * Testing
  , observableRollbackUseInTestsOnly
  ) where

import           Universum

import           Data.SafeCopy (base, deriveSafeCopy)

import           Test.QuickCheck (Arbitrary (..))

import           Formatting (bprint, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJsonIndent)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Control.Lens (each)
import           Pos.Chain.Txp (Utxo)
import qualified Pos.Core as Core
import           Pos.Core.Chrono (OldestFirst (..))
import qualified Pos.Core.Txp as Txp
import           Pos.Crypto (hash)

import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..))

import           Cardano.Wallet.Kernel.DB.BlockMeta
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import           Cardano.Wallet.Kernel.DB.Spec.Util
import           Cardano.Wallet.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'newPending'
data NewPendingFailed =
    -- | Some inputs are not in the wallet utxo
    NewPendingInputsUnavailable (Set (InDb Txp.TxIn))

deriveSafeCopy 1 'base ''NewPendingFailed

instance Buildable NewPendingFailed where
    build (NewPendingInputsUnavailable inputs) =
        let curatedInputs = map (view fromDb) (Set.toList inputs)
        in bprint ("NewPendingInputsUnavailable { inputs = " % listJsonIndent 4 % " }") curatedInputs

-- NOTE(adn) Short-circuiting the rabbit-hole with this instance by generating
-- an empty set, thus avoiding the extra dependency on @cardano-sl-core-test@.
instance Arbitrary NewPendingFailed where
    arbitrary = pure . NewPendingInputsUnavailable $ mempty

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
newPending :: InDb Txp.TxAux
           -> Update' Checkpoints NewPendingFailed ()
newPending tx = do
    checkpoints <- get
    let available' = available (checkpoints ^. currentUtxo) (checkpoints ^. currentPendingTxs)
    if isValidPendingTx tx' available'
        then
            put (insertPending checkpoints)
        else
            inputUnavailableErr available'

    where
        tx' = tx ^. fromDb

        insertPending :: Checkpoints -> Checkpoints
        insertPending cs = cs & currentPendingTxs %~ Map.insert txId tx'
            where txId = hash $ Txp.taTx tx'

        inputUnavailableErr available_ = do
            let unavailableInputs = txAuxInputSet tx' `Set.difference` utxoInputs available_
            throwError $ NewPendingInputsUnavailable (Set.map InDb unavailableInputs)

-- | Cancel the input set of cancelled transactions from @all@ the 'Checkpoints'
-- of an 'Account'.
cancelPending :: Set Txp.TxId -> Checkpoints -> Checkpoints
cancelPending txids checkpoints =
    checkpoints & over each
                (\ckpoint ->
                    ckpoint & over checkpointPending
                            (removePending txids)
                )

-- | Apply the prefiltered block to the specified wallet
applyBlock :: PrefilteredBlock
           -> Checkpoints
           -> Checkpoints
applyBlock prefBlock checkpoints
    = Checkpoint {
          _checkpointUtxo           = InDb utxo''
        , _checkpointUtxoBalance    = InDb balance''
        , _checkpointPending        = Pending . InDb $ pending''
        , _checkpointBlockMeta      = blockMeta''
        } NE.<| checkpoints
    where
        utxo'        = checkpoints ^. currentUtxo
        utxoBalance' = checkpoints ^. currentUtxoBalance

        (utxo'', balance'') = updateUtxo      prefBlock (utxo', utxoBalance')
        pending''           = updatePending   prefBlock (checkpoints ^. currentPendingTxs)
        blockMeta''         = updateBlockMeta prefBlock (checkpoints ^. currentBlockMeta)

updateBlockMeta :: PrefilteredBlock -> BlockMeta -> BlockMeta
updateBlockMeta PrefilteredBlock{..} meta
    = meta `mappend` pfbMeta

-- | Update (utxo,balance) with the given prefiltered block
updateUtxo :: PrefilteredBlock -> (Utxo, Core.Coin) -> (Utxo, Core.Coin)
updateUtxo PrefilteredBlock{..} (currentUtxo', currentBalance')
    = (utxo', balance')
    where
        unionUtxo            = Map.union pfbOutputs currentUtxo'
        utxo'                = utxoRemoveInputs unionUtxo pfbInputs

        unionUtxoRestricted  = utxoRestrictToInputs unionUtxo pfbInputs
        balanceDelta         = balanceI pfbOutputs - balanceI unionUtxoRestricted
        currentBalanceI      = Core.coinToInteger currentBalance'
        balance'             = Core.unsafeIntegerToCoin $ currentBalanceI + balanceDelta

-- | Update the pending transactions with the given prefiltered block
updatePending :: PrefilteredBlock -> PendingTxs -> PendingTxs
updatePending PrefilteredBlock{..} =
    Map.filter (\t -> disjoint (txAuxInputSet t) pfbInputs)

-- | Rollback
--
-- For the base case, see section "Rollback -- Omitting checkpoints" in the
-- formal specification.
--
-- This is an internal function only, and not exported. See 'switchToFork'.
rollback :: Checkpoints -> Checkpoints
rollback (c :| [])      = c :| []
rollback (c :| c' : cs) = Checkpoint {
      _checkpointUtxo        = c' ^. checkpointUtxo
    , _checkpointUtxoBalance = c' ^. checkpointUtxoBalance
    , _checkpointBlockMeta   = c' ^. checkpointBlockMeta
    , _checkpointPending     = unionPending (c  ^. checkpointPending)
                                            (c' ^. checkpointPending)
    } :| cs

-- | Observable rollback, used in testing only
--
-- See 'switchToFork' for production use.
observableRollbackUseInTestsOnly :: Checkpoints -> Checkpoints
observableRollbackUseInTestsOnly = rollback

-- | Switch to a fork
switchToFork :: Int  -- ^ Number of blocks to rollback
             -> OldestFirst [] PrefilteredBlock  -- ^ Blocks to apply
             -> Checkpoints -> Checkpoints
switchToFork = \n bs -> applyBlocks (getOldestFirst bs) . rollbacks n
  where
    applyBlocks :: [PrefilteredBlock] -> Checkpoints -> Checkpoints
    applyBlocks []     = identity
    applyBlocks (b:bs) = applyBlocks bs . applyBlock b

    rollbacks :: Int -> Checkpoints -> Checkpoints
    rollbacks 0 = identity
    rollbacks n = rollbacks (n - 1) . rollback
