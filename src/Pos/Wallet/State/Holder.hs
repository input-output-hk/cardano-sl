{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Wallet.State.Holder
       ( WalletDB
       , runWalletDB
       ) where

import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Trans         (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Mockable                    (ChannelT, MFunctor',
                                              Mockable (liftMockable), Promise,
                                              SharedAtomicT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Context                 (WithNodeContext)
import           Pos.Slotting                (MonadSlots)
import           Pos.Ssc.Extra.MonadLD       (MonadSscLD)
import           Pos.Statistics              (MonadStats)
import           Pos.Util.JsonLog            (MonadJL)

import           Pos.Wallet.Context          (WithWalletContext)
import           Pos.Wallet.KeyStorage       (MonadKeys)
import           Pos.Wallet.State.State      (MonadWalletDB (..), WalletState)
import           Pos.WorkMode                ()

-- | Holder for web wallet data
newtype WalletDB m a = WalletDB
    { getWalletDB :: ReaderT WalletState m a
    } deriving (Functor, Applicative, Monad, MonadThrow,
                MonadCatch, MonadMask, MonadIO, MonadFail, HasLoggerName,
                WithNodeContext ssc,
                MonadSlots, MonadSscLD ssc, MonadFix,
                MonadJL, CanLog, MonadStats,
                MonadKeys, WithWalletContext, MonadTrans)

instance Monad m => WrappedM (WalletDB m) where
    type UnwrappedM (WalletDB m) = ReaderT WalletState m
    _WrappedM = iso getWalletDB WalletDB

type instance ThreadId (WalletDB m) = ThreadId m
type instance Promise (WalletDB m) = Promise m
type instance SharedAtomicT (WalletDB m) = SharedAtomicT m
type instance ChannelT (WalletDB m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (WalletDB m) (ReaderT WalletState m)
         , MFunctor' d (ReaderT WalletState m) m
         ) => Mockable d (WalletDB m) where
    liftMockable = liftMockableWrappedM
    --
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
