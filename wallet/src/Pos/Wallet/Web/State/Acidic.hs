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
       , GetAccountIds (..)
       , GetAccountMeta (..)
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
    , 'WS.getAccountIds
    , 'WS.getAccountMeta
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
    , 'WS.flushWalletStorage
    , 'WS.getWalletStorage
    ]
