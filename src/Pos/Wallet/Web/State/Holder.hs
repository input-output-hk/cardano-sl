{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Wallet.Web.State.Holder
       ( WalletWebDB (..)
       , runWalletWebDB
       , getWalletState
       ) where

import           Universum

import           Control.Lens                (iso)
import           Control.Monad.Trans         (MonadTrans (..))
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              Gauge, MFunctor', Mockable (liftMockable),
                                              Promise, SharedAtomicT, SharedExclusiveT,
                                              SharedExclusiveT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)

import           Pos.Communication.PeerState (WithPeerState)
import           Pos.Context                 (WithNodeContext)
import           Pos.DB                      (MonadDB)
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.Reporting.MemState      (MonadReportingMem)
import           Pos.Slotting                (MonadSlots, MonadSlotsData)
import           Pos.Txp                     (MonadTxpMem)
import           Pos.Update                  (MonadPollRead)

import           Pos.Wallet.Context          (WithWalletContext)
import           Pos.Wallet.KeyStorage       (MonadKeys)
import           Pos.Wallet.State            (MonadWalletDB)
import           Pos.Wallet.Web.State.State  (MonadWalletWebDB (..), WalletState)

-- | Holder for web wallet data
newtype WalletWebDB m a = WalletWebDB
    { getWalletWebDB :: ReaderT WalletState m a
    } deriving (Functor, Applicative, Monad, MonadThrow, MonadSlotsData, MonadDB,
                MonadCatch, MonadMask, MonadIO, MonadFail, HasLoggerName, WithPeerState,
                MonadWalletDB, WithWalletContext, MonadSlots, MonadTrans,
                CanLog, MonadKeys, WithNodeContext ssc, MonadPollRead,
                MonadTxpMem __, MonadDelegation, MonadReportingMem)

instance Monad m => WrappedM (WalletWebDB m) where
    type UnwrappedM (WalletWebDB m) = ReaderT WalletState m
    _WrappedM = iso getWalletWebDB WalletWebDB

type instance ThreadId (WalletWebDB m) = ThreadId m
type instance Promise (WalletWebDB m) = Promise m
type instance SharedAtomicT (WalletWebDB m) = SharedAtomicT m
type instance Counter (WalletWebDB m) = Counter m
type instance Distribution (WalletWebDB m) = Distribution m
type instance SharedExclusiveT (WalletWebDB m) = SharedExclusiveT m
type instance Gauge (WalletWebDB m) = Gauge m
type instance ChannelT (WalletWebDB m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (WalletWebDB m) (ReaderT WalletState m)
         , MFunctor' d (ReaderT WalletState m) m
         ) => Mockable d (WalletWebDB m) where
    liftMockable = liftMockableWrappedM

-- | Instance for generic web wallet class
instance Monad m => MonadWalletWebDB (WalletWebDB m) where
    getWalletWebState = WalletWebDB ask

-- | Execute `WalletWebDB` action with given `WalletState`
runWalletWebDB :: WalletState -> WalletWebDB m a -> m a
runWalletWebDB ws = flip runReaderT ws . getWalletWebDB

getWalletState :: Monad m => WalletWebDB m WalletState
getWalletState = WalletWebDB ask
