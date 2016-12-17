{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Pos.Wallet.State.Acidic
       (
         WalletState
       , closeState
       , openMemState
       , openState
       , query
       , tidyState
       , update

       , GetBlock (..)
       , GetUtxo (..)
       , GetTxHistory (..)
       ) where

import           Universum

import           Data.Acid                (EventResult, EventState, QueryEvent,
                                           UpdateEvent, makeAcidic)
import           Data.Default             (def)
import           Pos.Wallet.State.Storage (Storage)
import           Pos.Wallet.State.Storage as WS
import           Serokell.AcidState       (ExtendedState, closeExtendedState,
                                           openLocalExtendedState,
                                           openMemoryExtendedState, queryExtended,
                                           tidyExtendedState, updateExtended)

type WalletState = ExtendedState Storage

query
    :: (EventState event ~ Storage, QueryEvent event, MonadIO m)
    => WalletState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ Storage, UpdateEvent event, MonadIO m)
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

makeAcidic ''Storage
    [
      'WS.getBlock
    , 'WS.getUtxo
    , 'WS.getTxHistory
    ]
