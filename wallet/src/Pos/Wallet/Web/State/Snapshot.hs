{-# LANGUAGE TypeFamilies #-}

-- | Snapshot api for wallet-db

module Pos.Wallet.Web.State.Snapshot
       ( withSnapshot
       , SnapshotT (..)
       ) where

import           Universum

import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), defaultLiftBaseWith,
                                              defaultLiftWith, defaultRestoreM,
                                              defaultRestoreT)
import           System.Wlog                 (CanLog, HasLoggerName)

import           Pos.Client.Txp.Addresses    (MonadAddresses (..))
import           Pos.Wallet.Web.State.State  (DBWalletStorage,
                                              MonadWalletDBMempoolRead (..),
                                              MonadWalletDBRead (..), UnionWalletStorage)

newtype SnapshotT m a = SnapshotT
    { unSnapshotT :: ReaderT (DBWalletStorage, UnionWalletStorage) m a
    } deriving ( Functor, Applicative, Monad
               , MonadThrow, MonadCatch, MonadMask
               , MonadTrans, CanLog, HasLoggerName, MonadIO
               )

instance MonadReader ctx m => MonadReader ctx (SnapshotT m) where
    ask = SnapshotT $ lift ask
    local f (SnapshotT rd) =
        SnapshotT $ ReaderT $ local f . runReaderT rd

instance MonadIO m => MonadBase IO (SnapshotT m) where
    liftBase = liftIO

-- Copied from here https://hackage.haskell.org/package/monad-control-1.0.2.2/docs/Control-Monad-Trans-Control.html#g:2
instance MonadTransControl SnapshotT where
    type StT SnapshotT a = StT (ReaderT (DBWalletStorage, UnionWalletStorage)) a
    liftWith = defaultLiftWith SnapshotT unSnapshotT
    restoreT = defaultRestoreT SnapshotT

-- Copied from https://hackage.haskell.org/package/monad-control-1.0.2.2/docs/Control-Monad-Trans-Control.html#g:5
instance (MonadIO m, MonadBaseControl IO m) => MonadBaseControl IO (SnapshotT m) where
    type StM (SnapshotT m) a = ComposeSt SnapshotT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance Monad m => MonadWalletDBRead (SnapshotT m) where
    getDBWalletStorage = SnapshotT $ asks fst

instance Monad m => MonadWalletDBMempoolRead (SnapshotT m) where
    getWalletStorages = SnapshotT ask

instance MonadAddresses m => MonadAddresses (SnapshotT m) where
    type AddrData (SnapshotT m) = AddrData m
    getNewAddress = lift . getNewAddress
    getFakeChangeAddress = lift getFakeChangeAddress

withSnapshot :: MonadWalletDBMempoolRead m => SnapshotT m a -> m a
withSnapshot action = getWalletStorages >>= runReaderT (unSnapshotT action)
