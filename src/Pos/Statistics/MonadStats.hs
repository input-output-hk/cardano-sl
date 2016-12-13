{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
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
       , StatsT (..)
       , runStatsT
       , runStatsT'
       ) where

import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Morph         (hoist)
import           Control.Monad.Trans         (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadResponse (..),
                                              MonadTransfer (..), hoistRespCond)
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import qualified Data.Binary                 as Binary
import           Data.Maybe                  (fromMaybe)
import           Focus                       (Decision (Remove), alterM)
import           Serokell.Util               (show')
import qualified STMContainers.Map           as SM
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

#ifdef WITH_ROCKS
import qualified Pos.Modern.DB               as Modern
#endif
import           Pos.DHT                     (DHTResponseT, MonadDHT,
                                              MonadMessageDHT (..), WithDefaultMsgHeader)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.LocalData     (MonadSscLD (..))
import           Pos.State                   (MonadDB)
import           Pos.Statistics.StatEntry    (StatLabel (..))
import           Pos.Util.JsonLog            (MonadJL (..))


-- | `MonadStats` is a monad which has methods for stats collecting
class Monad m => MonadStats m where
    statLog   :: StatLabel l => l -> EntryType l -> m ()
    resetStat :: StatLabel l => l -> m ()

    default statLog :: (MonadTrans t, StatLabel l) => l -> EntryType l -> t m ()
    statLog label = lift . statLog label

    default resetStat :: (MonadTrans t, StatLabel l) => l -> t m ()
    resetStat = lift . resetStat

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
               MonadJL, CanLog
#ifdef WITH_ROCKS
               , Modern.MonadDB ssc
#endif
               )

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
    resetStat _ = pure ()
    logStatM _ _ = pure ()

type StatsMap = SM.Map Text LByteString

-- | Statistics wrapper around some monadic action to collect statistics
-- during execution of this action. Used in benchmarks.
newtype StatsT m a = StatsT
    { getStatsT :: ReaderT StatsMap m a  -- ^ action inside wrapper with collected statistics
    } deriving (Functor, Applicative, Monad, MonadTimed, MonadThrow, MonadCatch,
               MonadMask, MonadIO, MonadDB ssc, HasLoggerName, MonadDialog p,
               MonadDHT, MonadMessageDHT, MonadSlots, WithDefaultMsgHeader, MonadTrans,
               MonadJL, CanLog
#ifdef WITH_ROCKS
               , Modern.MonadDB ssc
#endif
               )

instance MonadTransControl StatsT where
    type StT StatsT a = StT (ReaderT StatsMap) a
    liftWith = defaultLiftWith StatsT getStatsT
    restoreT = defaultRestoreT StatsT

instance MonadBaseControl IO m => MonadBaseControl IO (StatsT m) where
    type StM (StatsT m) a = ComposeSt StatsT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

instance MonadBase IO m => MonadBase IO (StatsT m) where
    liftBase = lift . liftBase

instance MonadTransfer m => MonadTransfer (StatsT m) where
    sendRaw addr req = StatsT ask >>= \ctx -> lift $ sendRaw addr (hoist (runStatsT' ctx) req)
    listenRaw binding sink =
        StatsT $ fmap StatsT $ listenRaw binding $ hoistRespCond getStatsT sink
    close = lift . close

instance MonadResponse m => MonadResponse (StatsT m) where
    replyRaw dat = StatsT $ replyRaw (hoist getStatsT dat)
    closeR = lift closeR
    peerAddr = lift peerAddr

instance MonadSscLD ssc m => MonadSscLD ssc (StatsT m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

runStatsT :: MonadIO m => StatsT m a -> m a
runStatsT action = liftIO SM.newIO >>= flip runStatsT' action

runStatsT' :: StatsMap -> StatsT m a -> m a
runStatsT' statsMap action = runReaderT (getStatsT action) statsMap

instance (MonadIO m, MonadJL m) => MonadStats (StatsT m) where
    statLog label entry = do
        statsMap <- StatsT ask
        liftIO $ atomically $ SM.focus update (show' label) statsMap
        return ()
      where
        update = alterM $ \v -> return $ fmap Binary.encode $
            mappend entry . Binary.decode <$> v <|> Just entry

    resetStat label = do
        statsMap <- StatsT ask
        mval <- liftIO $ atomically $ SM.focus reset (show' label) statsMap
        let val = fromMaybe mempty $ Binary.decode <$> mval
        lift $ jlLog $ toJLEvent label val
      where
        reset old = return (old, Remove)
