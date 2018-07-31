-- | READ-only operations on the wallet-spec state
module Cardano.Wallet.Kernel.DB.Spec.Read (
    -- * Queries
    queryAccountTotalBalance
  , queryAccountUtxo
  , queryAccountAvailableUtxo
  , queryAccountAvailableBalance
    -- * Pure functions on checkpoints
  , cpAvailableUtxo
  , cpAvailableBalance
  , Availability(..)
  , cpCheckAvailable
  ) where

import           Universum

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Pos.Chain.Txp as Core
import qualified Pos.Core as Core

import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Read as HD
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
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

data Availability = AllAvailable | Unavailable (Set Core.TxIn)

-- | Check whether the specified inputs are all available
--
-- Returns the inputs that are not available, if any
cpCheckAvailable :: IsCheckpoint c => Set Core.TxIn -> c -> Availability
cpCheckAvailable ins c
  | Set.null unavailable = AllAvailable
  | otherwise            = Unavailable unavailable
  where
    unavailable = ins Set.\\ Map.keysSet (cpAvailableUtxo c)

-- | Balance of the available UTxO
cpAvailableBalance :: IsCheckpoint c => c -> Core.Coin
cpAvailableBalance c =
    fromMaybe subCoinErr balance'
  where
    pendingIns = Pending.txIns (c ^. cpPending)
    spentUtxo  = Core.utxoRestrictToInputs (c ^. cpUtxo) pendingIns
    balance'   = Core.subCoin (c ^. cpUtxoBalance) (Core.utxoBalance spentUtxo)
    subCoinErr = error "cpAvailableBalance: spent more than available?"

-- | Change outputs
--
-- Pending outputs paid back into addresses that belong to the wallet.
cpChange :: IsCheckpoint c => IxSet HdAddress -> c -> Core.Utxo
cpChange ours = Pending.change ours' . view cpPending
  where
    ours' :: Core.Address -> Bool
    ours' addr = IxSet.size (IxSet.getEQ addr ours) == 1

-- | Total balance (available balance plus change)
cpTotalBalance :: IsCheckpoint c => IxSet HdAddress -> c -> Core.Coin
cpTotalBalance ours c =
    Core.unsafeAddCoin availableBalance changeBalance
  where
    availableBalance = cpAvailableBalance c
    changeBalance    = Core.utxoBalance (cpChange ours c)

{-------------------------------------------------------------------------------
  Public queries on an account, as defined in the Wallet Spec
-------------------------------------------------------------------------------}

queryAccountTotalBalance :: HdAccountId -> HD.HdQueryErr UnknownHdAccount Core.Coin
queryAccountTotalBalance accountId db =
    cpTotalBalance <$> ourAddrs <*> checkpoint
  where
    checkpoint = HD.readHdAccountCurrentCheckpoint accountId db
    ourAddrs   = HD.readAddressesByAccountId       accountId db

queryAccountUtxo :: HdAccountId -> HD.HdQueryErr UnknownHdAccount Core.Utxo
queryAccountUtxo accountId db =
    view (pcheckpointUtxo . fromDb) <$> checkpoint
  where
    checkpoint = HD.readHdAccountCurrentCheckpoint accountId db

queryAccountAvailableUtxo :: HdAccountId -> HD.HdQueryErr UnknownHdAccount Core.Utxo
queryAccountAvailableUtxo accountId db =
    cpAvailableUtxo <$> checkpoint
  where
    checkpoint = HD.readHdAccountCurrentCheckpoint accountId db

queryAccountAvailableBalance :: HdAccountId
                             -> HD.HdQueryErr UnknownHdAccount Core.Coin
queryAccountAvailableBalance accountId db =
    cpAvailableBalance <$> checkpoint
  where
    checkpoint = HD.readHdAccountCurrentCheckpoint accountId db
