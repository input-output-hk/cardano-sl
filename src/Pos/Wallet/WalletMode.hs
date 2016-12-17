{-# LANGUAGE CPP              #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( TxMode
       , WalletMode
       , WalletRealMode
       ) where

import           Control.TimeWarp.Rpc  (Dialog, Transfer)
import           Pos.DHT.Model         (DHTPacking)
import           Pos.DHT.Real          (KademliaDHT)
import           Pos.WorkMode          (MinWorkMode, SocketState)

import           Pos.Wallet.Context    (ContextHolder, WithWalletContext)
import           Pos.Wallet.KeyStorage (KeyStorage, MonadKeys)
import           Pos.Wallet.State      (MonadWalletDB, WalletDB)

type TxMode m
    = ( MinWorkMode m
      , MonadWalletDB m
      , WithWalletContext m
      )

type WalletMode m
    = ( TxMode m
      , MonadKeys m
      )

---------------------------------------------------------------
-- Implementations of 'WalletMode'
---------------------------------------------------------------
type WalletRealMode = KademliaDHT
                      (KeyStorage
                       (WalletDB
                        (ContextHolder
                         (Dialog DHTPacking (Transfer SocketState)))))
