{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of 'MonadSlotsData' which uses 'MonadDB'.

module Pos.Slotting.DB
       ( DBSlotsData (..)
       ) where

import           Control.Concurrent.STM      (TVar)
import           Control.Lens                (iso)
import           Control.Lens                (makeLenses)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Data.Time.Units             (Microsecond)
import           Formatting                  (sformat, (%))
import           Mockable                    (ChannelT, Counter, CurrentTime,
                                              Distribution, Gauge, MFunctor',
                                              Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           System.Wlog                 (logNotice, modifyLoggerName)
import           Universum

import           Pos.Context.Class           (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.Slotting.Class          (MonadSlotsData (..))
import           Pos.Types                   (SlotId, slotIdF)
import           Pos.Util.JsonLog            (MonadJL)

import qualified Control.Concurrent.STM      as STM
import           Data.List                   ((!!))
import           Data.Time.Units             (Microsecond)
import           Formatting                  (int, sformat, (%))
import           NTP.Client                  (NtpClientSettings (..), startNtpClient)
import           NTP.Example                 ()
import           System.Wlog                 (WithLogger, logDebug)
import           Universum

import qualified Pos.Constants               as C
import           Pos.Context                 (NodeContext (..), getNodeContext)

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

-- | Monad transformer which provides 'SlottingData' using DB.
newtype DBSlotsData m a = DBSlotsData
    { runDBSlotsData :: m a
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
               )

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

type instance ThreadId (DBSlotsData m) = ThreadId m
type instance Promise (DBSlotsData m) = Promise m
type instance SharedAtomicT (DBSlotsData m) = SharedAtomicT m
type instance Counter (DBSlotsData m) = Counter m
type instance Distribution (DBSlotsData m) = Distribution m
type instance SharedExclusiveT (DBSlotsData m) = SharedExclusiveT m
type instance Gauge (DBSlotsData m) = Gauge m
type instance ChannelT (DBSlotsData m) = ChannelT m

instance MonadTrans DBSlotsData where
    lift = DBSlotsData

instance ( Mockable d m
         , MFunctor' d (DBSlotsData m) m
         ) => Mockable d (DBSlotsData m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (DBSlotsData m) where
    type UnwrappedM (DBSlotsData m) = m
    _WrappedM = iso runDBSlotsData DBSlotsData

instance MonadTransControl DBSlotsData where
    type StT DBSlotsData a = a
    liftWith f = DBSlotsData $ f $ runDBSlotsData
    restoreT = DBSlotsData

instance MonadBaseControl IO m => MonadBaseControl IO (DBSlotsData m) where
    type StM (DBSlotsData m) a = ComposeSt DBSlotsData m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

instance MonadDB σ m => MonadSlotsData (DBSlotsData m) where
    getSlottingData = notImplemented
