{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- @jens: this document is inspired by https://github.com/input-output-hk/rscoin-haskell/blob/master/src/RSCoin/Explorer/AcidState.hs

module Pos.Wallet.Web.AcidState
       (
         WalletState
       , closeState
       , openMemState
       , openState
       , query
       , tidyState
       , update

       , GetDummyAttribute (..)
       , SetDummyAttribute (..)
       ) where

import           Universum

import           Data.Acid              (EventResult, EventState, Query, QueryEvent,
                                         Update, UpdateEvent, makeAcidic)
import           Data.Default           (def)
import           Pos.Wallet.Web.Storage as WS
import           Serokell.AcidState     (ExtendedState, closeExtendedState,
                                         openLocalExtendedState, openMemoryExtendedState,
                                         queryExtended, tidyExtendedState, updateExtended)

type WalletState = ExtendedState WS.Storage

query
    :: (EventState event ~ WS.Storage, QueryEvent event, MonadIO m)
    => WalletState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ WS.Storage, UpdateEvent event, MonadIO m)
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

makeAcidic ''WS.Storage
    [
      'WS.getDummyAttribute
    , 'WS.setDummyAttribute
    ]
