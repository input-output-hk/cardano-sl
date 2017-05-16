{-# LANGUAGE TypeFamilies #-}

-- | Monadic layer for collecting stats

module Pos.Statistics.MonadStats
       ( MonadStats (..)
       , NoStatsT
       , getNoStatsT
       , StatsT
       , StatsMap
       , runStatsT
       , runStatsT'
       , getStatsMap
       ) where

import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Identity (IdentityT (..))
import qualified Data.Binary                  as Binary
import           Data.Coerce                  (coerce)
import qualified Ether
import           Focus                        (Decision (Remove), alterM)
import           Serokell.Util                (show')
import qualified STMContainers.Map            as SM
import           Universum

import           Pos.Statistics.StatEntry     (StatLabel (..))
import           Pos.Util.JsonLog             (MonadJL, jlLog)
import           Pos.Util.Util                (ether)


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

instance {-# OVERLAPPABLE #-}
    (MonadStats m, MonadTrans t, Monad (t m)) =>
        MonadStats (t m)

data NoStatsTag

-- | Stats wrapper for collecting statistics without collecting it.
type NoStatsT = Ether.TaggedTrans NoStatsTag IdentityT

getNoStatsT :: NoStatsT m a -> m a
getNoStatsT = coerce

instance Monad m => MonadStats (NoStatsT m) where
    statLog _ _ = pure ()
    resetStat _ = pure ()
    logStatM _ _ = pure ()

type StatsMap = SM.Map Text LByteString

-- | Statistics wrapper around some monadic action to collect statistics
-- during execution of this action. Used in benchmarks.
type StatsT = Ether.ReaderT' StatsMap

runStatsT :: MonadIO m => StatsT m a -> m a
runStatsT action = liftIO SM.newIO >>= flip runStatsT' action

runStatsT' :: StatsMap -> StatsT m a -> m a
runStatsT' = flip Ether.runReaderT

getStatsMap :: Monad m => StatsT m StatsMap
getStatsMap = Ether.ask'

instance (MonadIO m, MonadJL m) => MonadStats (StatsT m) where
    statLog label entry = do
        statsMap <- ether ask
        atomically $ SM.focus update (show' label) statsMap
        return ()
      where
        update = alterM $ \v -> return $ fmap Binary.encode $
            mappend entry . Binary.decode <$> v <|> Just entry

    resetStat label = do
        statsMap <- ether ask
        mval <- atomically $ SM.focus reset (show' label) statsMap
        let val = fromMaybe mempty $ Binary.decode <$> mval
        lift $ jlLog $ toJLEvent label val
      where
        reset old = return (old, Remove)
