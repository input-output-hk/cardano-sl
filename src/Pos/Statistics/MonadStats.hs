{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Monadic layer for collecting stats

module Pos.Statistics.MonadStats
       ( MonadStats (..)
       , StatEntry
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
import           Control.TimeWarp.Rpc     (MonadDialog, MonadResponse, MonadTransfer (..),
                                           hoistRespCond)
import           Control.TimeWarp.Timed   (MonadTimed (..), ThreadId)
import           Universum

import           Pos.DHT                  (DHTResponseT, MonadDHT, MonadMessageDHT (..),
                                           WithDefaultMsgHeader)
import           Pos.DHT.Real             (KademliaDHT)
import           Pos.Slotting             (MonadSlots (..))
import           Pos.State                (MonadDB (..), addStatRecord, getStatRecords)
import           Pos.Types                (Timestamp (..))

type CounterLabel = Text
type StatEntry = (LByteString, Timestamp)

class Monad m => MonadStats m where
    logStat :: CounterLabel -> StatEntry -> m ()
    getStats :: CounterLabel -> m (Maybe [StatEntry])

    -- | Default convenience method, which we can override
    -- (to truly do nothing in `NoStatsT`, for example)
    logStatM :: CounterLabel -> m StatEntry -> m ()
    logStatM label action = action >>= logStat label

-- TODO: is there a way to avoid such boilerplate for transformers?
instance MonadStats m => MonadStats (KademliaDHT m) where
    logStat label = lift . logStat label
    getStats = lift . getStats

instance MonadStats m => MonadStats (ReaderT a m) where
    logStat label = lift . logStat label
    getStats = lift . getStats

instance MonadStats m => MonadStats (StateT a m) where
    logStat label = lift . logStat label
    getStats = lift . getStats

instance MonadStats m => MonadStats (ExceptT e m) where
    logStat label = lift . logStat label
    getStats = lift . getStats

instance MonadStats m => MonadStats (DHTResponseT m) where
    logStat label = lift . logStat label
    getStats = lift . getStats

type instance ThreadId (NoStatsT m) = ThreadId m
type instance ThreadId (StatsT m) = ThreadId m

newtype NoStatsT m a = NoStatsT
    { getNoStatsT :: m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB, WithNamedLogger, MonadDialog p,
               MonadDHT, MonadMessageDHT, MonadResponse, MonadSlots, WithDefaultMsgHeader)

instance MonadTransfer m => MonadTransfer (NoStatsT m) where
    sendRaw addr p = NoStatsT $ sendRaw addr p
    listenRaw binding sink = NoStatsT $ listenRaw binding (hoistRespCond getNoStatsT sink)
    close = NoStatsT . close

instance MonadTrans NoStatsT where
    lift = NoStatsT

instance Monad m => MonadStats (NoStatsT m) where
    logStat _ _ = pure ()
    getStats _ = pure $ pure []
    logStatM _ _ = pure ()

newtype StatsT m a = StatsT
    { getStatsT :: m a
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB, WithNamedLogger, MonadDialog p,
               MonadDHT, MonadMessageDHT, MonadResponse, MonadSlots, WithDefaultMsgHeader)

instance MonadTransfer m => MonadTransfer (StatsT m) where
    sendRaw addr p = StatsT $ sendRaw addr p
    listenRaw binding sink = StatsT $ listenRaw binding (hoistRespCond getStatsT sink)
    close = StatsT . close

instance MonadTrans StatsT where
    lift = StatsT

instance (MonadIO m, MonadDB m) => MonadStats (StatsT m) where
    logStat label = lift . uncurry (addStatRecord label)
    getStats = lift . getStatRecords
