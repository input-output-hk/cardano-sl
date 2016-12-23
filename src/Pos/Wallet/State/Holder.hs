{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.Wallet.State.Holder
       ( WalletDB
       , runWalletDB
       ) where

import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Trans         (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadTransfer)
import           Control.TimeWarp.Timed      (MonadTimed, ThreadId)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.DHT.Model               (MonadDHT, MonadMessageDHT,
                                              WithDefaultMsgHeader)
import           Pos.Slotting                (MonadSlots)
import           Pos.Ssc.Class               (MonadSscLD)
import           Pos.State                   (MonadDB)
import           Pos.Statistics              (MonadStats)
import           Pos.Txp.LocalData           (MonadTxLD)
import           Pos.Util.JsonLog            (MonadJL)

import           Pos.Wallet.Context          (WithWalletContext)
import           Pos.Wallet.KeyStorage       (MonadKeys)
import           Pos.Wallet.State.State      (MonadWalletDB (..), WalletState)
import           Pos.WorkMode                ()

-- | Holder for web wallet data
newtype WalletDB m a = WalletDB
    { getWalletDB :: ReaderT WalletState m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
                MonadMask, MonadIO, MonadDB ssc, HasLoggerName, WithNodeContext ssc,
                MonadDialog s p, MonadDHT, MonadMessageDHT s, MonadSlots, MonadSscLD ssc,
                WithDefaultMsgHeader, MonadJL, CanLog, MonadTxLD, MonadStats, MonadKeys,
                WithWalletContext)

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

instance MonadBase IO m => MonadBase IO (WalletDB m) where
    liftBase = lift . liftBase

instance MonadTransControl WalletDB where
    type StT WalletDB a = StT (ReaderT WalletState) a
    liftWith = defaultLiftWith WalletDB getWalletDB
    restoreT = defaultRestoreT WalletDB

instance MonadBaseControl IO m => MonadBaseControl IO (WalletDB m) where
    type StM (WalletDB m) a = ComposeSt WalletDB m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

-- | Execute `WalletDB` action with given `WalletState`
runWalletDB :: WalletState -> WalletDB m a -> m a
runWalletDB ws = flip runReaderT ws . getWalletDB
