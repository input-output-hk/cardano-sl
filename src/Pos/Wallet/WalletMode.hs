{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( TxMode
       , WalletMode
       , WalletRealMode
       , SState
       ) where

import           Control.TimeWarp.Rpc          (Dialog, Transfer)
import           Pos.Communication.Types.State (MutSocketState)
import           Pos.DHT.Model                 (DHTPacking)
import           Pos.DHT.Real                  (KademliaDHT)
import           Pos.Ssc.GodTossing            (SscGodTossing)
import           Pos.WorkMode                  (MinWorkMode)

import           Pos.Wallet.Context            (ContextHolder, WithWalletContext)
import           Pos.Wallet.KeyStorage         (KeyStorage, MonadKeys)
import           Pos.Wallet.State              (MonadWalletDB, WalletDB)

type SState = MutSocketState SscGodTossing

type TxMode m
    = ( MinWorkMode SState m
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
                         (Dialog DHTPacking (Transfer SState)))))
