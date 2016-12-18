{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( TxMode
       , WalletMode
       , WalletRealMode
       ) where

import           Pos.Communication.Types (MutSocketState)
import           Pos.Context             (WithNodeContext)
import qualified Pos.Modern.DB           as Modern
import           Pos.Ssc.Class.Storage   (SscStorageMode)
import           Pos.State               (MonadDB)
import           Pos.Txp.LocalData       (MonadTxLD)
import           Pos.WorkMode            (MinWorkMode, RawRealMode)

import           Pos.Wallet.KeyStorage   (KeyStorage, MonadKeys)

type TxMode ssc m
    = ( MinWorkMode (MutSocketState ssc) m
      , MonadDB ssc m
      , Modern.MonadDB ssc m
      , MonadTxLD m
      , WithNodeContext ssc m
      , SscStorageMode ssc
      )

type WalletMode ssc m
    = ( TxMode ssc m
      , MonadKeys m
      )

---------------------------------------------------------------
-- Implementations of 'WalletMode'
---------------------------------------------------------------
type WalletRealMode ssc = KeyStorage (RawRealMode ssc)
