{-# LANGUAGE TypeFamilies #-}
-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/AcidState.hs

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
       , GetAccountMetas (..)
       , GetAccountMeta (..)
       , GetWalletMetas (..)
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
       , GetUpdates (..)
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
       , GetWalletTxHistory (..)
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
       , GetWalletStorage (..)
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

type WalletState = ExtendedState WalletStorage

query
    :: (EventState event ~ WalletStorage, QueryEvent event, MonadIO m)
    => WalletState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ WalletStorage, UpdateEvent event, MonadIO m)
    => WalletState -> event -> m (EventResult event)
update = updateExtended

openState :: (MonadIO m, HasConfiguration) => Bool -> FilePath -> m WalletState
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp def

openMemState :: (MonadIO m, HasConfiguration) => m WalletState
openMemState = openMemoryExtendedState def

closeState :: MonadIO m => WalletState -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => WalletState -> m ()
tidyState = tidyExtendedState

makeAcidic ''WalletStorage
    [
      'WS.testReset
    , 'WS.getProfile
    , 'WS.doesAccountExist
    , 'WS.getAccountIds
    , 'WS.getAccountMetas
    , 'WS.getAccountMeta
    , 'WS.getWalletMetas
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
    , 'WS.getUpdates
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
    , 'WS.getWalletTxHistory
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
    , 'WS.flushWalletStorage
    , 'WS.getWalletStorage
    ]
