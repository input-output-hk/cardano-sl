{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Slotting in wallet.

module Pos.Wallet.Slotting
       ( WalletDBSlotsData (..)
       ) where

import           Control.Concurrent.STM      (TVar)
import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (iso)
import           Control.Lens                (makeLenses)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Data.List                   ((!!))
import           Data.Time.Units             (Microsecond)
import           Data.Time.Units             (Microsecond)
import           Formatting                  (sformat, (%))
import           Formatting                  (int, sformat, (%))
import           Mockable                    (ChannelT, Counter, CurrentTime,
                                              Distribution, Gauge, MFunctor',
                                              Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              liftMockableWrappedM)
import           NTP.Client                  (NtpClientSettings (..), startNtpClient)
import           NTP.Example                 ()
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           System.Wlog                 (logNotice, modifyLoggerName)
import           System.Wlog                 (WithLogger, logDebug)
import           Universum

import           Universum

import qualified Pos.Constants               as C
import           Pos.Context                 (NodeContext (..), getNodeContext)
import           Pos.Context.Class           (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.Slotting.Class          (MonadSlotsData (..))
import           Pos.Types                   (SlotId, slotIdF)
import           Pos.Util.JsonLog            (MonadJL)

import           Pos.Wallet.State.State      (MonadWalletDB)

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

-- | Monad transformer which provides 'SlottingData' using Wallet's DB.
newtype WalletDBSlotsData m a = WalletDBSlotsData
    { runWalletDBSlotsData :: m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadFix

               , MonadThrow
               , MonadCatch
               , MonadMask

               , MonadBase base

               , HasLoggerName
               , CanLog

               , MonadDB σ
               , WithNodeContext ssc
               , MonadJL
               , MonadWalletDB
               )

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

type instance ThreadId (WalletDBSlotsData m) = ThreadId m
type instance Promise (WalletDBSlotsData m) = Promise m
type instance SharedAtomicT (WalletDBSlotsData m) = SharedAtomicT m
type instance Counter (WalletDBSlotsData m) = Counter m
type instance Distribution (WalletDBSlotsData m) = Distribution m
type instance SharedExclusiveT (WalletDBSlotsData m) = SharedExclusiveT m
type instance Gauge (WalletDBSlotsData m) = Gauge m
type instance ChannelT (WalletDBSlotsData m) = ChannelT m

instance MonadTrans WalletDBSlotsData where
    lift = WalletDBSlotsData

instance ( Mockable d m
         , MFunctor' d (WalletDBSlotsData m) m
         ) => Mockable d (WalletDBSlotsData m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (WalletDBSlotsData m) where
    type UnwrappedM (WalletDBSlotsData m) = m
    _WrappedM = iso runWalletDBSlotsData WalletDBSlotsData

instance MonadTransControl WalletDBSlotsData where
    type StT WalletDBSlotsData a = a
    liftWith f = WalletDBSlotsData $ f $ runWalletDBSlotsData
    restoreT = WalletDBSlotsData

instance MonadBaseControl IO m => MonadBaseControl IO (WalletDBSlotsData m) where
    type StM (WalletDBSlotsData m) a = ComposeSt WalletDBSlotsData m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

instance Monad m => MonadSlotsData (WalletDBSlotsData m) where
    getSlottingData = undefined
    waitSlottingData = undefined
    putSlottingData = undefined
