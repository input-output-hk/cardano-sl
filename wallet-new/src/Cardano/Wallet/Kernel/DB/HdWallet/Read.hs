{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

-- | READ queries on the HD wallet
--
-- NOTE: These are pure functions, which are intended to work on a snapshot
-- of the database. They are intended to support the V1 wallet API.
module Cardano.Wallet.Kernel.DB.HdWallet.Read (
    -- | Summarize
    accountsByRootId
  , addressesByRootId
  , addressesByAccountId
  , pendingByAccount
    -- | Simple lookups
  , lookupHdRootId
  , lookupHdAccountId
  , lookupHdAddressId
  , lookupCardanoAddress
    -- | Properties of an entire root
  , rootAssuranceLevel
  , rootTotalBalance
    -- | Queries on an account's current checkpoint
  , currentUtxo
  , currentAvailableUtxo
  , currentTotalBalance
  , currentAvailableBalance
  , currentAddressMeta
  , currentTxSlotId
  , currentTxIsPending
  ) where

import           Universum

import           Control.Lens (to)

import           Pos.Chain.Txp (TxId, Utxo)
import           Pos.Core (Address, Coin, SlotId, mkCoin, unsafeAddCoin)

import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta)
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Spec (IsCheckpoint (..),
                     cpAddressMeta)
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import           Cardano.Wallet.Kernel.DB.Spec.Read
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet (Indexed, IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Summarize
-------------------------------------------------------------------------------}

-- | All accounts in the given wallet
--
-- NOTE: Does not check that the root exists.
accountsByRootId :: HdRootId -> Query' e HdWallets (IxSet HdAccount)
accountsByRootId rootId = do
    asks $ IxSet.getEQ rootId . view hdWalletsAccounts

-- | All addresses in the given wallet
--
-- NOTE: Does not check that the root exists.
addressesByRootId :: HdRootId -> Query' e HdWallets (IxSet (Indexed HdAddress))
addressesByRootId rootId =
    asks $ IxSet.getEQ rootId . view hdWalletsAddresses

-- | All addresses in the given account
--
-- NOTE: Does not check that the account exists.
addressesByAccountId :: HdAccountId -> Query' e HdWallets (IxSet (Indexed HdAddress))
addressesByAccountId accId =
    asks $ IxSet.getEQ accId . view hdWalletsAddresses

-- | All pending transactions in all accounts
pendingByAccount :: Query' e HdWallets (Map HdAccountId Pending)
pendingByAccount = fmap aux . IxSet.toMap <$> view hdWalletsAccounts
  where
    aux :: HdAccount -> Pending
    aux acc = acc ^. hdAccountState . hdAccountStateCurrent cpPending

{-------------------------------------------------------------------------------
  Simple lookups
-------------------------------------------------------------------------------}

lookupHdRootId :: HdRootId -> Query' UnknownHdRoot HdWallets HdRoot
lookupHdRootId rootId = zoomHdRootId identity rootId $ ask

lookupHdAccountId :: HdAccountId -> Query' UnknownHdAccount HdWallets HdAccount
lookupHdAccountId accId = zoomHdAccountId identity accId $ ask

lookupHdAddressId :: HdAddressId -> Query' UnknownHdAddress HdWallets HdAddress
lookupHdAddressId addrId = zoomHdAddressId identity addrId $ ask

lookupCardanoAddress :: Address -> Query' UnknownHdAddress HdWallets HdAddress
lookupCardanoAddress addr = zoomHdCardanoAddress identity addr $ ask

{-------------------------------------------------------------------------------
  Properties of an entire HdRoot
-------------------------------------------------------------------------------}

rootAssuranceLevel :: HdRootId -> Query' UnknownHdRoot HdWallets AssuranceLevel
rootAssuranceLevel rootId =
    zoomHdRootId identity rootId $
      view hdRootAssurance

-- | Total balance for all accounts in the given root
--
-- NOTE: Does not check that the root exists.
rootTotalBalance :: HdRootId -> Query' e HdWallets Coin
rootTotalBalance rootId = do
    accounts <- IxSet.getEQ rootId <$> view hdWalletsAccounts
    sumTotals <$> mapM currentTotalBalance' (IxSet.toList accounts)
  where
    sumTotals :: [Coin] -> Coin
    sumTotals = foldl' unsafeAddCoin (mkCoin 0)

{-------------------------------------------------------------------------------
  Functions on the most recent checkpoint
-------------------------------------------------------------------------------}

-- | Internal: lift a function on the current checkpoint
liftCP :: (forall c. IsCheckpoint c => c -> a)
       -> HdAccountId -> Query' UnknownHdAccount HdWallets a
liftCP f accId =
    zoomHdAccountId identity accId $
      zoomHdAccountCurrent $
        asks f

currentUtxo :: HdAccountId -> Query' UnknownHdAccount HdWallets Utxo
currentUtxo = liftCP (view cpUtxo)

currentAvailableUtxo :: HdAccountId -> Query' UnknownHdAccount HdWallets Utxo
currentAvailableUtxo = liftCP cpAvailableUtxo

currentTxSlotId :: TxId -> HdAccountId -> Query' UnknownHdAccount HdWallets (Maybe SlotId)
currentTxSlotId txId = liftCP $ cpTxSlotId txId

currentTxIsPending :: TxId -> HdAccountId -> Query' UnknownHdAccount HdWallets Bool
currentTxIsPending txId = liftCP $ cpTxIsPending txId

currentAvailableBalance :: HdAccountId -> Query' UnknownHdAccount HdWallets Coin
currentAvailableBalance = liftCP cpAvailableBalance

currentAddressMeta :: HdAddress -> Query' UnknownHdAccount HdWallets AddressMeta
currentAddressMeta = withAddr $ \addr ->
    liftCP $ view (cpAddressMeta (addr ^. hdAddressAddress . fromDb))
  where
    withAddr :: (HdAddress -> HdAccountId -> Query' e st a)
             -> (HdAddress -> Query' e st a)
    withAddr f addr = f addr (addr ^. hdAddressId . hdAddressIdParent)

currentTotalBalance :: HdAccountId -> Query' UnknownHdAccount HdWallets Coin
currentTotalBalance accId =
    zoomHdAccountId identity accId ask >>= currentTotalBalance'

-- Internal helper generalization
--
-- Total balance breaks the pattern because we need the set of addresses that
-- belong to the account, but for that we need 'HdWallets'.
currentTotalBalance' :: HdAccount -> Query' e HdWallets Coin
currentTotalBalance' acc = do
    ourAddrs <- IxSet.getEQ (acc ^. hdAccountId) <$> view hdWalletsAddresses
    return (acc ^. hdAccountState . hdAccountStateCurrent (to $ cpTotalBalance ourAddrs))
