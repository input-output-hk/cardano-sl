{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
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
       , GetWalletAddresses (..)
       , GetWalletMetas (..)
       , GetWalletMeta (..)
       , GetWSetMetas (..)
       , GetWSetMeta (..)
       , GetWSetPassLU (..)
       , GetWSetSyncTip (..)
       , GetWSetAddresses (..)
       , GetWalletAccounts (..)
       , DoesAccountExist (..)
       , GetTxMeta (..)
       , GetUpdates (..)
       , GetNextUpdate (..)
       , TestReset (..)
       , GetHistoryCache (..)
       , CreateWallet (..)
       , AddAccount (..)
       , AddRemovedAccount (..)
       , CreateWSet (..)
       , SetProfile (..)
       , SetWalletMeta (..)
       , SetWSetMeta (..)
       , SetWSetPassLU (..)
       , SetWSetSyncTip (..)
       , SetWalletTransactionMeta (..)
       , SetWalletHistory (..)
       , GetWalletHistory (..)
       , AddOnlyNewTxMeta (..)
       , RemoveWSet (..)
       , RemoveWallet (..)
       , RemoveAccount (..)
       , TotallyRemoveAccount (..)
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
    , 'WS.getWalletAddresses
    , 'WS.getWalletMetas
    , 'WS.getWalletMeta
    , 'WS.getWSetMetas
    , 'WS.getWSetMeta
    , 'WS.getWSetPassLU
    , 'WS.getWSetSyncTip
    , 'WS.getWSetAddresses
    , 'WS.getWalletAccounts
    , 'WS.doesAccountExist
    , 'WS.getTxMeta
    , 'WS.getUpdates
    , 'WS.getNextUpdate
    , 'WS.getHistoryCache
    , 'WS.createWallet
    , 'WS.createWSet
    , 'WS.addAccount
    , 'WS.addRemovedAccount
    , 'WS.setProfile
    , 'WS.setWalletMeta
    , 'WS.setWSetMeta
    , 'WS.setWSetPassLU
    , 'WS.setWSetSyncTip
    , 'WS.setWalletTransactionMeta
    , 'WS.setWalletHistory
    , 'WS.getWalletHistory
    , 'WS.addOnlyNewTxMeta
    , 'WS.removeWSet
    , 'WS.removeWallet
    , 'WS.removeAccount
    , 'WS.totallyRemoveAccount
    , 'WS.addUpdate
    , 'WS.removeNextUpdate
    , 'WS.updateHistoryCache
    ]
