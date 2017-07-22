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
       , GetAccountMetas (..)
       , GetAccountMeta (..)
       , GetWalletMetas (..)
       , GetWalletMeta (..)
       , GetWalletPassLU (..)
       , GetWalletSyncTip (..)
       , GetWalletAddresses (..)
       , GetWalletUtxo (..)
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
       , AddCustomAddress (..)
       , CreateAccount (..)
       , AddWAddress (..)
       , AddRemovedAccount (..)
       , CreateWallet (..)
       , SetProfile (..)
       , SetAccountMeta (..)
       , SetWalletMeta (..)
       , SetWalletPassLU (..)
       , SetWalletSyncTip (..)
       , SetWalletTxMeta (..)
       , SetWalletTxHistory (..)
       , GetWalletTxHistory (..)
       , AddOnlyNewTxMeta (..)
       , RemoveWallet (..)
       , RemoveTxMetas (..)
       , RemoveHistoryCache (..)
       , RemoveAccount (..)
       , RemoveWAddress (..)
       , RemoveCustomAddress (..)
       , TotallyRemoveWAddress (..)
       , AddUpdate (..)
       , RemoveNextUpdate (..)
       , UpdateHistoryCache (..)
       ) where

import           Universum

import           Data.Acid                    (EventResult, EventState, QueryEvent,
                                               UpdateEvent, makeAcidic)
import           Data.Default                 (def)
import           Pos.Wallet.Web.State.Storage (WalletStorage)
import           Pos.Wallet.Web.State.Storage as WS
import           Serokell.AcidState           (ExtendedState, closeExtendedState,
                                               openLocalExtendedState,
                                               openMemoryExtendedState, queryExtended,
                                               tidyExtendedState, updateExtended)

type WalletState = ExtendedState WalletStorage

query
    :: (EventState event ~ WalletStorage, QueryEvent event, MonadIO m)
    => WalletState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ WalletStorage, UpdateEvent event, MonadIO m)
    => WalletState -> event -> m (EventResult event)
update = updateExtended

openState :: MonadIO m => Bool -> FilePath -> m WalletState
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp def

openMemState :: MonadIO m => m WalletState
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
    , 'WS.getAccountMetas
    , 'WS.getAccountMeta
    , 'WS.getWalletMetas
    , 'WS.getWalletMeta
    , 'WS.getWalletPassLU
    , 'WS.getWalletSyncTip
    , 'WS.getWalletAddresses
    , 'WS.getWalletUtxo
    , 'WS.setWalletUtxo
    , 'WS.getAccountWAddresses
    , 'WS.doesWAddressExist
    , 'WS.getTxMeta
    , 'WS.getUpdates
    , 'WS.getNextUpdate
    , 'WS.getHistoryCache
    , 'WS.getCustomAddresses
    , 'WS.getCustomAddress
    , 'WS.addCustomAddress
    , 'WS.removeCustomAddress
    , 'WS.createAccount
    , 'WS.createWallet
    , 'WS.addWAddress
    , 'WS.addRemovedAccount
    , 'WS.setProfile
    , 'WS.setAccountMeta
    , 'WS.setWalletMeta
    , 'WS.setWalletPassLU
    , 'WS.setWalletSyncTip
    , 'WS.setWalletTxMeta
    , 'WS.setWalletTxHistory
    , 'WS.getWalletTxHistory
    , 'WS.addOnlyNewTxMeta
    , 'WS.removeWallet
    , 'WS.removeTxMetas
    , 'WS.removeHistoryCache
    , 'WS.removeAccount
    , 'WS.removeWAddress
    , 'WS.totallyRemoveWAddress
    , 'WS.addUpdate
    , 'WS.removeNextUpdate
    , 'WS.updateHistoryCache
    ]
