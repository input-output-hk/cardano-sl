{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Monadic layer for collecting stats

module Pos.Statistics.MonadStats
       ( MonadStats (..)
       , NoStatsT (..)
       , StatsT (..)
       , runStatsT
       , runStatsT'
       ) where

import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.Morph         (hoist)
import           Control.Monad.Trans         (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import qualified Data.Binary                 as Binary
import           Data.Maybe                  (fromMaybe)
import           Focus                       (Decision (Remove), alterM)
import           Mockable                    (ChannelT, MFunctor',
                                              Mockable (liftMockable), Promise,
                                              SharedAtomicT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util               (WrappedM (..), show')
import qualified STMContainers.Map           as SM
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Communication.PeerState (WithPeerState (..))
import           Pos.Context.Class           (WithNodeContext)
import           Pos.DB                      (MonadDB (..))
import           Pos.Delegation.Class        (MonadDelegation)
import           Pos.NewDHT.Model            (MonadDHT)
import           Pos.NewDHT.Real             (KademliaDHT)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Extra               (MonadSscGS (..), MonadSscLD (..))
import           Pos.Statistics.StatEntry    (StatLabel (..))
import           Pos.Txp.Class               (MonadTxpLD (..))
import           Pos.Types                   (MonadUtxo, MonadUtxoRead)
import           Pos.Util.JsonLog            (MonadJL (..))


-- | `MonadStats` is a monad which has methods for stats collecting
class Monad m => MonadStats m where
    statLog   :: StatLabel l => l -> EntryType l -> m ()
    resetStat :: StatLabel l => l -> m ()

    default statLog :: (MonadTrans t, MonadStats m', t m' ~ m, StatLabel l) => l -> EntryType l -> m ()
    statLog label = lift . statLog label

    default resetStat :: (MonadTrans t, MonadStats m', t m' ~ m, StatLabel l) => l -> m ()
    resetStat = lift . resetStat

    -- | Default convenience method, which we can override
    -- (to truly do nothing in `NoStatsT`, for example)
    logStatM :: StatLabel l => l -> m (EntryType l) -> m ()
    logStatM label action = action >>= statLog label

instance MonadStats m => MonadStats (KademliaDHT    m)
instance MonadStats m => MonadStats (ReaderT      a m)
instance MonadStats m => MonadStats (StateT       a m)
instance MonadStats m => MonadStats (ExceptT      e m)

type instance ThreadId (NoStatsT m) = ThreadId m
type instance ThreadId (StatsT m) = ThreadId m

-- | Stats wrapper for collecting statistics without collecting it.
newtype NoStatsT m a = NoStatsT
    { getNoStatsT :: m a  -- ^ action inside wrapper without collecting statistics
    } deriving (Functor, Applicative, Monad, MonadThrow,
                MonadCatch, MonadMask, MonadIO, MonadFail, HasLoggerName,
                MonadDHT, MonadSlots, WithPeerState ssc,
                MonadJL, CanLog,
                MonadUtxoRead, MonadUtxo,
                MonadTxpLD ssc, MonadSscGS ssc, MonadSscLD ssc,
                WithNodeContext ssc, MonadDelegation)

deriving instance MonadDB ssc m => MonadDB ssc (NoStatsT m)

instance Monad m => WrappedM (NoStatsT m) where
    type UnwrappedM (NoStatsT m) = m
    _WrappedM = iso getNoStatsT NoStatsT

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

instance MonadTrans NoStatsT where
    lift = NoStatsT

instance Monad m => MonadStats (NoStatsT m) where
    statLog _ _ = pure ()
    resetStat _ = pure ()
    logStatM _ _ = pure ()

type StatsMap = SM.Map Text LByteString

type instance ThreadId (NoStatsT m) = ThreadId m
type instance Promise (NoStatsT m) = Promise m
type instance SharedAtomicT (NoStatsT m) = SharedAtomicT m
type instance ChannelT (NoStatsT m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (NoStatsT m) m
         ) => Mockable d (NoStatsT m) where
    liftMockable = liftMockableWrappedM

-- | Statistics wrapper around some monadic action to collect statistics
-- during execution of this action. Used in benchmarks.
newtype StatsT m a = StatsT
    { getStatsT :: ReaderT StatsMap m a  -- ^ action inside wrapper with collected statistics
    } deriving (Functor, Applicative, Monad, MonadThrow,
                MonadCatch, MonadMask, MonadIO, MonadFail, HasLoggerName,
                MonadDHT, MonadSlots, WithPeerState ssc,
                MonadTrans, MonadJL, CanLog,
                MonadUtxoRead, MonadUtxo, MonadTxpLD ssc,
                MonadSscGS ssc, MonadSscLD ssc, WithNodeContext ssc, MonadDelegation)

deriving instance MonadDB ssc m => MonadDB ssc (StatsT m)
instance Monad m => WrappedM (StatsT m) where
    type UnwrappedM (StatsT m) = ReaderT StatsMap m
    _WrappedM = iso getStatsT StatsT

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

type instance ThreadId (StatsT m) = ThreadId m
type instance Promise (StatsT m) = Promise m
type instance SharedAtomicT (StatsT m) = SharedAtomicT m
type instance ChannelT (StatsT m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (StatsT m) (ReaderT StatsMap m)
         , MFunctor' d (ReaderT StatsMap m) m
         ) => Mockable d (StatsT m) where
    liftMockable = liftMockableWrappedM

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
