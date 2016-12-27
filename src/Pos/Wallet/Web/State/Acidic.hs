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

       , GetWalletMetas (..)
       , GetWalletMeta (..)
       , CreateWallet (..)
       , SetWalletMeta (..)
       , SetWalletTransactionMeta (..)
       , SetWalletHistory (..)
       , GetWalletHistory (..)
       , AddOnlyNewTxMeta (..)
       , RemoveWallet (..)
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
      'WS.getWalletMetas
    , 'WS.getWalletMeta
    , 'WS.createWallet
    , 'WS.setWalletMeta
    , 'WS.setWalletTransactionMeta
    , 'WS.setWalletHistory
    , 'WS.getWalletHistory
    , 'WS.addOnlyNewTxMeta
    , 'WS.removeWallet
    ]
