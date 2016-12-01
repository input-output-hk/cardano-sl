{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Monadic layer for collecting stats

module Pos.Statistics.MonadStats
       ( MonadStats (..)
       , NoStatsT (..)
       , getNoStatsT
       , StatsT (..)
       , getStatsT
       ) where

import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Morph         (hoist)
import           Control.Monad.Trans         (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultRestoreM)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadResponse (..),
                                              MonadTransfer (..), hoistRespCond)
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import qualified Data.Binary                 as Binary
import           Data.Maybe                  (fromMaybe)
import           Focus                       (Decision (Remove), alterM)
import           Serokell.Util               (show')
import qualified STMContainers.Map           as SM
import           System.IO.Unsafe            (unsafePerformIO)
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.DHT                     (DHTResponseT, MonadDHT,
                                              MonadMessageDHT (..), WithDefaultMsgHeader)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.LocalData     (MonadSscLD (..))
import           Pos.State                   (MonadDB)
import           Pos.Statistics.StatEntry    (StatLabel (..))
import           Pos.Types                   (Timestamp (..))
import           Pos.Util.JsonLog            (MonadJL (..))


type Stats l = Maybe [(Timestamp, EntryType l)]

-- | `MonadStats` is a monad which has methods for stats collecting
class Monad m => MonadStats m where
    statLog   :: StatLabel l => l -> EntryType l -> m ()
    resetStat :: StatLabel l => l -> m ()
    getStats  :: StatLabel l => l -> m (Stats l)

    default statLog :: (MonadTrans t, StatLabel l) => l -> EntryType l -> t m ()
    statLog label = lift . statLog label

    default resetStat :: (MonadTrans t, StatLabel l) => l -> t m ()
    resetStat = lift . resetStat

    default getStats :: (MonadTrans t, StatLabel l) => l -> t m (Stats l)
    getStats = lift . getStats

    -- | Default convenience method, which we can override
    -- (to truly do nothing in `NoStatsT`, for example)
    logStatM :: StatLabel l => l -> m (EntryType l) -> m ()
    logStatM label action = action >>= statLog label

instance MonadStats m => MonadStats (KademliaDHT    m)
instance MonadStats m => MonadStats (ReaderT      a m)
instance MonadStats m => MonadStats (StateT       a m)
instance MonadStats m => MonadStats (ExceptT      e m)
instance MonadStats m => MonadStats (DHTResponseT   m)

type instance ThreadId (NoStatsT m) = ThreadId m
type instance ThreadId (StatsT m) = ThreadId m

-- | Stats wrapper for collecting statistics without collecting it.
newtype NoStatsT m a = NoStatsT
    { getNoStatsT :: m a  -- ^ action inside wrapper without collecting statistics
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB ssc, HasLoggerName, MonadDialog p,
               MonadDHT, MonadMessageDHT, MonadSlots, WithDefaultMsgHeader,
               MonadJL, CanLog)

instance MonadBase IO m => MonadBase IO (NoStatsT m) where
    liftBase = lift . liftBase

instance MonadTransControl NoStatsT where
    type StT NoStatsT a = a
    liftWith f = NoStatsT $ f $ getNoStatsT
    restoreT = NoStatsT

instance MonadBaseControl IO m => MonadBaseControl IO (NoStatsT m) where
    type StM (NoStatsT m) a = ComposeSt NoStatsT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance MonadTransfer m => MonadTransfer (NoStatsT m) where
    sendRaw addr p = NoStatsT $ sendRaw addr (hoist getNoStatsT p)
    listenRaw binding sink = NoStatsT $ fmap NoStatsT $ listenRaw binding (hoistRespCond getNoStatsT sink)
    close = NoStatsT . close

instance MonadResponse m => MonadResponse (NoStatsT m) where
    replyRaw dat = NoStatsT $ replyRaw (hoist getNoStatsT dat)
    closeR = lift closeR
    peerAddr = lift peerAddr

instance MonadSscLD ssc m => MonadSscLD ssc (NoStatsT m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

instance MonadTrans NoStatsT where
    lift = NoStatsT

instance Monad m => MonadStats (NoStatsT m) where
    statLog _ _ = pure ()
    getStats _ = pure $ pure []
    resetStat _ = pure ()
    logStatM _ _ = pure ()

-- | Statistics wrapper around some monadic action to collect statistics
-- during execution of this action. Used in benchmarks.
newtype StatsT m a = StatsT
    { getStatsT :: m a  -- ^ action inside wrapper with collected statistics
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB ssc, HasLoggerName, MonadDialog p,
               MonadDHT, MonadMessageDHT, MonadSlots, WithDefaultMsgHeader,
               MonadJL, CanLog)

instance MonadBase IO m => MonadBase IO (StatsT m) where
    liftBase = lift . liftBase

instance MonadTransControl StatsT where
    type StT StatsT a = a
    liftWith f = StatsT $ f $ getStatsT
    restoreT = StatsT

instance MonadBaseControl IO m => MonadBaseControl IO (StatsT m) where
    type StM (StatsT m) a = ComposeSt StatsT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance MonadTransfer m => MonadTransfer (StatsT m) where
    sendRaw addr p = StatsT $ sendRaw addr (hoist getStatsT p)
    listenRaw binding sink = StatsT $ fmap StatsT $ listenRaw binding (hoistRespCond getStatsT sink)
    close = StatsT . close

instance MonadResponse m => MonadResponse (StatsT m) where
    replyRaw dat = StatsT $ replyRaw (hoist getStatsT dat)
    closeR = lift closeR
    peerAddr = lift peerAddr

instance MonadSscLD ssc m => MonadSscLD ssc (StatsT m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

instance MonadTrans StatsT where
    lift = StatsT

-- [CSL-196]: Global mutable variables are bad
statsMap :: SM.Map Text LByteString
statsMap = unsafePerformIO SM.newIO

instance (MonadIO m, MonadJL m) => MonadStats (StatsT m) where
    statLog label entry = do
        liftIO $ atomically $ SM.focus update (show' label) statsMap
        return ()
      where
        update = alterM $ \v -> return $ fmap Binary.encode $
            mappend entry . Binary.decode <$> v <|> Just entry

    resetStat label = do
        mval <- liftIO $ atomically $ SM.focus reset (show' label) statsMap
        let val = fromMaybe mempty $ Binary.decode <$> mval
        lift $ jlLog $ toJLEvent label val
      where
        reset old = return (old, Remove)

    -- [CSL-196]: do we need getStats at all?
    getStats _ = pure $ pure []
