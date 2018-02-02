{-# LANGUAGE TypeFamilies #-}
-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/AcidState.hs

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
       , AddRemovedAccount (..)
       , CreateWallet (..)
       , SetProfile (..)
       , SetAccountMeta (..)
       , SetWalletMeta (..)
       , SetWalletReady (..)
       , SetWalletPassLU (..)
       , SetWalletSyncTip (..)
       , SetWalletTxMeta (..)
       , AddOnlyNewTxMetas (..)
       , SetWalletTxHistory (..)
       , AddOnlyNewTxMeta (..)
       , RemoveWallet (..)
       , RemoveTxMetas (..)
       , RemoveWalletTxMetas (..)
       , RemoveHistoryCache (..)
       , RemoveAccount (..)
       , RemoveWAddress (..)
       , RemoveCustomAddress (..)
       , TotallyRemoveWAddress (..)
       , AddUpdate (..)
       , RemoveNextUpdate (..)
       , InsertIntoHistoryCache (..)
       , RemoveFromHistoryCache (..)
       , SetPtxCondition (..)
       , CasPtxCondition (..)
       , PtxUpdateMeta (..)
       , AddOnlyNewPendingTx (..)
       , CancelApplyingPtxs (..)
       , CancelSpecificApplyingPtx (..)
       , FlushWalletStorage (..)
       -- * No longer used, just here for migrations and backwards compatibility
       , UpdateHistoryCache (..)
       ) where

import           Universum

import           Data.Acid                    (EventResult, EventState, QueryEvent,
                                               UpdateEvent, makeAcidic)
import           Data.Default                 (def)
import           Serokell.AcidState           (ExtendedState, closeExtendedState,
                                               openLocalExtendedState,
                                               openMemoryExtendedState, queryExtended,
                                               tidyExtendedState, updateExtended)

import           Pos.Core.Configuration       (HasConfiguration)
import           Pos.Wallet.Web.State.Storage (WalletStorage)
import           Pos.Wallet.Web.State.Storage as WS

type WalletDB = ExtendedState WalletStorage

query
    :: (EventState event ~ WalletStorage, QueryEvent event, MonadIO m)
    => WalletDB -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ WalletStorage, UpdateEvent event, MonadIO m)
    => WalletDB -> event -> m (EventResult event)
update = updateExtended

openState :: (MonadIO m, HasConfiguration) => Bool -> FilePath -> m WalletDB
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp def

openMemState :: (MonadIO m, HasConfiguration) => m WalletDB
openMemState = openMemoryExtendedState def

closeState :: MonadIO m => WalletDB -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => WalletDB -> m ()
tidyState = tidyExtendedState

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
    , 'WS.addRemovedAccount
    , 'WS.setProfile
    , 'WS.setAccountMeta
    , 'WS.setWalletMeta
    , 'WS.setWalletReady
    , 'WS.setWalletPassLU
    , 'WS.setWalletSyncTip
    , 'WS.setWalletTxMeta
    , 'WS.addOnlyNewTxMetas
    , 'WS.setWalletTxHistory
    , 'WS.addOnlyNewTxMeta
    , 'WS.removeWallet
    , 'WS.removeTxMetas
    , 'WS.removeWalletTxMetas
    , 'WS.removeHistoryCache
    , 'WS.removeAccount
    , 'WS.removeWAddress
    , 'WS.totallyRemoveWAddress
    , 'WS.addUpdate
    , 'WS.removeNextUpdate
    , 'WS.updateHistoryCache
    , 'WS.insertIntoHistoryCache
    , 'WS.removeFromHistoryCache
    , 'WS.setPtxCondition
    , 'WS.casPtxCondition
    , 'WS.ptxUpdateMeta
    , 'WS.addOnlyNewPendingTx
    , 'WS.cancelApplyingPtxs
    , 'WS.cancelSpecificApplyingPtx
    , 'WS.flushWalletStorage
    , 'WS.getWalletStorage
    ]
