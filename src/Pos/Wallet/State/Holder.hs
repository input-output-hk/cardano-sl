{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.Wallet.State.Holder
       ( WalletDB
       , runWalletDB
       ) where

import           Universum

import           Control.Lens           (iso)
import           Control.Monad.Trans    (MonadTrans (..))
import           Control.TimeWarp.Rpc   (MonadDialog, MonadTransfer)
import           Control.TimeWarp.Timed (MonadTimed, ThreadId)
import           Serokell.Util.Lens     (WrappedM (..))
import           System.Wlog            (CanLog, HasLoggerName)

import           Pos.Context            (WithNodeContext)
import           Pos.DHT.Model          (MonadDHT, MonadMessageDHT, WithDefaultMsgHeader)
import           Pos.Slotting           (MonadSlots)
import           Pos.Ssc.Class          (MonadSscLD)
import           Pos.State              (MonadDB)
import           Pos.Statistics         (MonadStats)
import           Pos.Txp.LocalData      (MonadTxLD)
import           Pos.Util.JsonLog       (MonadJL)
import           Pos.Wallet.KeyStorage  (MonadKeys)
import           Pos.Wallet.State.State (MonadWalletDB (..), WalletState)
import           Pos.WorkMode           ()

-- | Holder for web wallet data
newtype WalletDB m a = WalletDB
    { getWalletDB :: ReaderT WalletState m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
                MonadMask, MonadIO, MonadDB ssc, HasLoggerName, WithNodeContext ssc,
                MonadDialog s p, MonadDHT, MonadMessageDHT s, MonadSlots, MonadSscLD ssc,
                WithDefaultMsgHeader, MonadJL, CanLog, MonadTxLD, MonadStats, MonadKeys)

instance Monad m => WrappedM (WalletDB m) where
    type UnwrappedM (WalletDB m) = ReaderT WalletState m
    _WrappedM = iso getWalletDB WalletDB

instance MonadTrans WalletDB where
    lift = WalletDB . lift

instance MonadTransfer s m => MonadTransfer s (WalletDB m)

type instance ThreadId (WalletDB m) = ThreadId m

-- | Instance for generic web wallet class
instance Monad m => MonadWalletDB (WalletDB m) where
    getWalletState = WalletDB ask

-- | Execute `WalletDB` action with given `WalletState`
runWalletDB :: WalletState -> WalletDB m a -> m a
runWalletDB ws = flip runReaderT ws . getWalletDB
