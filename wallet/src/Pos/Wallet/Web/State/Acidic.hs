{-# LANGUAGE TypeFamilies #-}

-- | A module which derives acidic events from actions defined
-- in "Pos.Wallet.Web.State.Storage".
module Pos.Wallet.Web.State.Acidic
       (
         WalletDB
       , closeState
       , openMemState
       , openState
       , query
       , tidyState
       , update

         -- * Only query transaction
       , GetWalletStorage (..)

         -- * All the update transactions
       , DoesAccountExist (..)
       , UpdateWalletBalancesAndUtxo (..)
       , SetWalletUtxo (..)
       , DoesWAddressExist (..)
       , TestReset (..)
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
       , SetWalletRestorationSyncTip (..)
       , UpdateSyncStatistics (..)
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
       , FlushWalletStorage (..)
       -- * No longer used, just here for migrations and backwards compatibility
       , UpdateHistoryCache (..)
       -- * Grouped transactions
       , CreateAccountWithAddress (..)
       , RemoveWallet2 (..)
       , ApplyModifierToWallet (..)
       , ApplyModifierToWallet2 (..)
       , RollbackModifierFromWallet (..)
       , RollbackModifierFromWallet2 (..)
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
import           Pos.Wallet.Web.State.Transactions as WST

type WalletDB = ExtendedState WalletStorage

query
    :: (EventState event ~ WalletStorage, QueryEvent event, MonadIO m)
    => WalletDB -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ WalletStorage, UpdateEvent event, MonadIO m)
    => WalletDB -> event -> m (EventResult event)
update = updateExtended

-- | Initialize wallet DB in disk mode. Used in production.
openState :: (MonadIO m, HasConfiguration) => Bool -> FilePath -> m WalletDB
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp def

-- | Initialize empty wallet DB in pure (in-memory) mode.
-- Used primarily for testing.
openMemState :: (MonadIO m, HasConfiguration) => m WalletDB
openMemState = openMemoryExtendedState def

-- | Close wallet DB resource.
closeState :: MonadIO m => WalletDB -> m ()
closeState = closeExtendedState

-- | Compress current event log, create a new checkpoint and delete old checkpoints.
tidyState :: MonadIO m => WalletDB -> m ()
tidyState = tidyExtendedState

-- TH derivations of acidic events

makeAcidic ''WalletStorage
    [
      'WS.testReset
    , 'WS.doesAccountExist
    , 'WS.updateWalletBalancesAndUtxo
    , 'WS.setWalletUtxo
    , 'WS.doesWAddressExist
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
    , 'WS.setWalletRestorationSyncTip
    , 'WS.updateSyncStatistics
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
    , 'WST.createAccountWithAddress
    , 'WST.removeWallet2
    , 'WST.applyModifierToWallet
    , 'WST.applyModifierToWallet2
    , 'WST.rollbackModifierFromWallet
    , 'WST.rollbackModifierFromWallet2
    ]
