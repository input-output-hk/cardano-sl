-- | Functions on checkpoints
module Cardano.Wallet.Kernel.DB.Spec.Read (
    -- * Pure functions on checkpoints
    cpAvailableUtxo
  , cpAvailableBalance
  , cpCheckAvailable
  , cpChange
  , cpTotalBalance
  , cpTxSlotId
  , cpTxIsPending
  ) where

import           Universum

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core

import           Cardano.Wallet.API.V1.Types (V1 (..))
import           Cardano.Wallet.Kernel.DB.BlockMeta (blockMetaSlotId,
                     localBlockMeta)
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.Util.IxSet (Indexed, IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Cardano.Wallet.Kernel.Util.Core as Core

{-------------------------------------------------------------------------------
  Pure functions that support read-only operations on an account Checkpoint, as
  defined in the Wallet Spec
-------------------------------------------------------------------------------}

-- | Available UTxO
--
-- The available UtxO is the current UTxO minus outputs spent by pending txs
cpAvailableUtxo :: IsCheckpoint c => c -> Core.Utxo
cpAvailableUtxo c =
    Core.utxoRemoveInputs (c ^. cpUtxo) pendingIns
  where
    pendingIns = Pending.txIns (c ^. cpPending)

-- | Returns the sets of available and unavailable inputs
cpCheckAvailable :: IsCheckpoint c
                 => Set Core.TxIn -> c -> (Set Core.TxIn, Set Core.TxIn)
cpCheckAvailable ins c = Set.partition isAvailable ins
  where
    isAvailable :: Core.TxIn -> Bool
    isAvailable inp = inp `Map.member` cpAvailableUtxo c

-- | Balance of the available UTxO
cpAvailableBalance :: IsCheckpoint c => c -> Core.Coin
cpAvailableBalance c =
    fromMaybe subCoinErr balance'
  where
    pendingIns   = Pending.txIns (c ^. cpPending)
    spentUtxo    = Core.utxoRestrictToInputs (c ^. cpUtxo) pendingIns
    spentBalance = Core.unsafeIntegerToCoin $ Core.utxoBalance spentUtxo
    balance'     = Core.subCoin (c ^. cpUtxoBalance) spentBalance
    subCoinErr   = error "cpAvailableBalance: spent more than available?"

-- | Change outputs
--
-- Pending outputs paid back into addresses that belong to the wallet.
cpChange :: IsCheckpoint c => IxSet (Indexed HdAddress) -> c -> Core.Utxo
cpChange ours cp =
    Map.union
      (Pending.change ours' $ cp ^. cpPending)
      (Pending.change ours' $ cp ^. cpForeign)
  where
    ours' :: Core.Address -> Bool
    ours' addr = IxSet.size (IxSet.getEQ (V1 addr) ours) == 1

-- | Total balance (available balance plus change)
cpTotalBalance :: IsCheckpoint c => IxSet (Indexed HdAddress) -> c -> Core.Coin
cpTotalBalance ours c =
    Core.unsafeAddCoin availableBalance changeBalance
  where
    availableBalance = cpAvailableBalance c
    changeBalance    = Core.unsafeIntegerToCoin $
                         Core.utxoBalance (cpChange ours c)

-- | SlotId a transaction got confirmed in
cpTxSlotId :: IsCheckpoint c => Core.TxId -> c -> Maybe Core.SlotId
cpTxSlotId txId c = Map.lookup txId slots
  where
    blockMeta = localBlockMeta (c ^. cpBlockMeta)
    slots     = view (blockMetaSlotId . fromDb) blockMeta

-- | Check if a transaction is pending
cpTxIsPending :: IsCheckpoint c => Core.TxId -> c -> Bool
cpTxIsPending txId c = Pending.member txId (c ^. cpPending)
