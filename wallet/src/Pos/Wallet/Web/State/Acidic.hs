{-# LANGUAGE TypeFamilies #-}

-- | A module which derives acidic events from actions defined
-- in "Pos.Wallet.Web.State.Storage".
module Pos.Wallet.Web.State.Acidic
       (
         WalletState
       , closeState
       , openMemState
       , openState
       , query
       , tidyState
       , update

       , GetProfile (..)
       , DoesAccountExist (..)
       , GetAccountIds (..)
       , GetAccountMeta (..)
       , GetAccountAddrMaps (..)
       , GetWalletMeta (..)
       , GetWalletMetaIncludeUnready (..)
       , GetWalletPassLU (..)
       , GetWalletSyncTip (..)
       , GetWalletAddresses (..)
       , GetWalletUtxo (..)
       , GetWalletBalancesAndUtxo (..)
       , UpdateWalletBalancesAndUtxo (..)
       , SetWalletUtxo (..)
       , GetAccountWAddresses (..)
       , DoesWAddressExist (..)
       , GetTxMeta (..)
       , GetNextUpdate (..)
       , TestReset (..)
       , GetHistoryCache (..)
       , GetCustomAddresses (..)
       , GetCustomAddress (..)
       , GetPendingTxs (..)
       , GetWalletPendingTxs (..)
       , GetPendingTx (..)
       , AddCustomAddress (..)
       , CreateAccount (..)
       , AddWAddress (..)
       , CreateWallet (..)
       , SetProfile (..)
       , SetAccountMeta (..)
       , SetWalletMeta (..)
       , SetWalletReady (..)
       , SetWalletPassLU (..)
       , SetWalletSyncTip (..)
       , SetWalletTxMeta (..)
       , AddOnlyNewTxMetas (..)
       , GetWalletTxHistory (..)
       , AddOnlyNewTxMeta (..)
       , RemoveWallet (..)
       , RemoveTxMetas (..)
       , RemoveWalletTxMetas (..)
       , RemoveHistoryCache (..)
       , RemoveAccount (..)
       , RemoveWAddress (..)
       , RemoveCustomAddress (..)
       , AddUpdate (..)
       , RemoveNextUpdate (..)
       , InsertIntoHistoryCache (..)
       , RemoveFromHistoryCache (..)
       , SetPtxCondition (..)
       , CasPtxCondition (..)
       , RemoveOnlyCreatingPtx (..)
       , PtxUpdateMeta (..)
       , AddOnlyNewPendingTx (..)
       , ResetFailedPtxs (..)
       , CancelApplyingPtxs (..)
       , CancelSpecificApplyingPtx (..)
       , GetWalletStorage (..)
       , FlushWalletStorage (..)
       -- * No longer used, just here for migrations and backwards compatibility
       , UpdateHistoryCache (..)
       ) where

import           Universum

import           Data.Acid (EventResult, EventState, QueryEvent, UpdateEvent, makeAcidic)
import           Data.Default (def)
import           Serokell.AcidState (ExtendedState, closeExtendedState, openLocalExtendedState,
                                     openMemoryExtendedState, queryExtended, tidyExtendedState,
                                     updateExtended)

import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Wallet.Web.State.Storage (WalletStorage)
import           Pos.Wallet.Web.State.Storage as WS

-- | Type alias for acidic state which contains 'WalletStorage'.
type WalletState = ExtendedState WalletStorage

query
    :: (EventState event ~ WalletStorage, QueryEvent event, MonadIO m)
    => WalletState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ WalletStorage, UpdateEvent event, MonadIO m)
    => WalletState -> event -> m (EventResult event)
update = updateExtended

-- | Initialize wallet DB in disk mode. Used in production.
openState :: (MonadIO m, HasConfiguration) => Bool -> FilePath -> m WalletState
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp def

-- | Initialize empty wallet DB in pure (in-memory) mode.
-- Used primarily for testing.
openMemState :: (MonadIO m, HasConfiguration) => m WalletState
openMemState = openMemoryExtendedState def

-- | Close wallet DB resource.
closeState :: MonadIO m => WalletState -> m ()
closeState = closeExtendedState

-- | Compress current event log, create a new checkpoint and delete old checkpoints.
tidyState :: MonadIO m => WalletState -> m ()
tidyState = tidyExtendedState

-- TH derivations of acidic events

makeAcidic ''WalletStorage
    [
      'WS.testReset
    , 'WS.getProfile
    , 'WS.doesAccountExist
    , 'WS.getAccountIds
    , 'WS.getAccountMeta
    , 'WS.getAccountAddrMaps
    , 'WS.getWalletMeta
    , 'WS.getWalletMetaIncludeUnready
    , 'WS.getWalletPassLU
    , 'WS.getWalletSyncTip
    , 'WS.getWalletAddresses
    , 'WS.getWalletUtxo
    , 'WS.getWalletBalancesAndUtxo
    , 'WS.updateWalletBalancesAndUtxo
    , 'WS.setWalletUtxo
    , 'WS.getAccountWAddresses
    , 'WS.doesWAddressExist
    , 'WS.getTxMeta
    , 'WS.getNextUpdate
    , 'WS.getHistoryCache
    , 'WS.getCustomAddresses
    , 'WS.getCustomAddress
    , 'WS.getPendingTxs
    , 'WS.getWalletPendingTxs
    , 'WS.getPendingTx
    , 'WS.addCustomAddress
    , 'WS.removeCustomAddress
    , 'WS.createAccount
    , 'WS.createWallet
    , 'WS.addWAddress
    , 'WS.setProfile
    , 'WS.setAccountMeta
    , 'WS.setWalletMeta
    , 'WS.setWalletReady
    , 'WS.setWalletPassLU
    , 'WS.setWalletSyncTip
    , 'WS.setWalletTxMeta
    , 'WS.addOnlyNewTxMetas
    , 'WS.getWalletTxHistory
    , 'WS.addOnlyNewTxMeta
    , 'WS.removeWallet
    , 'WS.removeTxMetas
    , 'WS.removeWalletTxMetas
    , 'WS.removeHistoryCache
    , 'WS.removeAccount
    , 'WS.removeWAddress
    , 'WS.addUpdate
    , 'WS.removeNextUpdate
    , 'WS.updateHistoryCache
    , 'WS.insertIntoHistoryCache
    , 'WS.removeFromHistoryCache
    , 'WS.setPtxCondition
    , 'WS.casPtxCondition
    , 'WS.removeOnlyCreatingPtx
    , 'WS.ptxUpdateMeta
    , 'WS.addOnlyNewPendingTx
    , 'WS.resetFailedPtxs
    , 'WS.cancelApplyingPtxs
    , 'WS.cancelSpecificApplyingPtx
    , 'WS.flushWalletStorage
    , 'WS.getWalletStorage
    ]
