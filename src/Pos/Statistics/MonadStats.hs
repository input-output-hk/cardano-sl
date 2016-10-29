{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Monadic layer for collecting stats

module Pos.Statistics.MonadStats
       ( MonadStats (..)
       , NoStatsT
       , CounterLabel
       , getNoStatsT
       , StatsT
       , getStatsT
       ) where

import           Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except     (ExceptT)
import           Control.Monad.Trans      (MonadTrans)
import           Control.TimeWarp.Logging (WithNamedLogger (..))
import           Control.TimeWarp.Rpc     (MonadDialog, MonadResponse, MonadTransfer)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId)
import           Data.Binary              (Binary)
import           Data.MessagePack         (MessagePack)
import           Pos.DHT                  (DHTResponseT, MonadDHT, MonadMessageDHT (..),
                                           WithDefaultMsgHeader)
import           Pos.DHT.Real             (KademliaDHT)
import           Pos.Slotting             (MonadSlots (..))
import           Pos.State                (MonadDB (..), NodeState, addStatRecord,
                                           getStatRecords)
import           Pos.Types                (Timestamp (..))
import           Universum


type CounterLabel = Text
type GoodStatEntry a
    = ( Typeable a
      , Binary a
      , MessagePack a
      )

class (Monad m, GoodStatEntry (StatEntry m)) => MonadStats m where
    type StatEntry m :: *

    logStat :: CounterLabel -> StatEntry m -> m ()
    getStats :: CounterLabel -> m (Maybe [StatEntry m])

-- TODO: is there a way to avoid such boilerplate for transformers?
instance MonadStats m => MonadStats (KademliaDHT m) where
    type StatEntry (KademliaDHT m) = StatEntry m
    logStat label = lift . logStat label
    getStats = lift . getStats

instance MonadStats m => MonadStats (ReaderT a m) where
    type StatEntry (ReaderT a m) = StatEntry m
    logStat label = lift . logStat label
    getStats = lift . getStats

instance MonadStats m => MonadStats (StateT a m) where
    type StatEntry (StateT a m) = StatEntry m
    logStat label = lift . logStat label
    getStats = lift . getStats

instance MonadStats m => MonadStats (ExceptT e m) where
    type StatEntry (ExceptT e m) = StatEntry m
    logStat label = lift . logStat label
    getStats = lift . getStats

instance MonadStats m => MonadStats (DHTResponseT m) where
    type StatEntry (DHTResponseT m) = StatEntry m
    logStat label = lift . logStat label
    getStats = lift . getStats

type instance ThreadId (NoStatsT m) = ThreadId m
type instance ThreadId (StatsT m) = ThreadId m

newtype NoStatsT m a = NoStatsT
    { getNoStatsT :: m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB, WithNamedLogger, MonadTransfer, MonadDialog,
               MonadDHT, MonadMessageDHT, MonadResponse, MonadSlots, WithDefaultMsgHeader)

instance MonadTrans NoStatsT where
    lift = NoStatsT

instance Monad m => MonadStats (NoStatsT m) where
    type StatEntry (NoStatsT m) = ()
    logStat _ _ = pure ()
    getStats _ = pure $ pure []

newtype StatsT m a = StatsT
    { getStatsT :: m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB, WithNamedLogger, MonadTransfer, MonadDialog,
               MonadDHT, MonadMessageDHT, MonadResponse, MonadSlots, WithDefaultMsgHeader)

instance MonadTrans StatsT where
    lift = StatsT

instance (MonadIO m, MonadDB m) => MonadStats (StatsT m) where
    type StatEntry (StatsT m) = (ByteString, Timestamp)
    logStat label = lift . uncurry (addStatRecord label)
    getStats = lift . getStatRecords
