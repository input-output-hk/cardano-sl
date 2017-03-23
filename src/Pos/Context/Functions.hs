-- | Functions operating on NodeContext.

module Pos.Context.Functions
       (
         -- * Genesis
         genesisUtxoM
       , genesisLeadersM

         -- * Block semaphore.
       , putBlkSemaphore
       , readBlkSemaphore
       , takeBlkSemaphore

         -- * LRC synchronization
       , waitLrc
       , lrcActionOnEpoch
       , lrcActionOnEpochReason

         -- * Misc
       , getUptime
       , isRecoveryMode
       ) where

import qualified Control.Concurrent.STM as STM
import           Data.Time              (diffUTCTime, getCurrentTime)
import           Data.Time.Units        (Microsecond, fromMicroseconds)
import           Universum

import           Pos.Context.Class      (WithNodeContext (..))
import           Pos.Context.Context    (NodeContext (..), ncGenesisLeaders,
                                         ncGenesisUtxo, ncStartTime)
import           Pos.Lrc.Context        (LrcContext (..), LrcSyncData (..))
import           Pos.Lrc.Error          (LrcError (..))
import           Pos.Txp.Toil.Types     (Utxo)
import           Pos.Types              (EpochIndex, HeaderHash, SlotLeaders)
import           Pos.Util               (maybeThrow, readTVarConditional)
import           Pos.Util.Context       (HasContext, askContext)

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

genesisUtxoM :: (Functor m, WithNodeContext ssc m) => m Utxo
genesisUtxoM = ncGenesisUtxo <$> getNodeContext

genesisLeadersM :: (Functor m, WithNodeContext ssc m) => m SlotLeaders
genesisLeadersM = ncGenesisLeaders <$> getNodeContext

----------------------------------------------------------------------------
-- Semaphore-related logic
----------------------------------------------------------------------------

takeBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => m HeaderHash
takeBlkSemaphore = liftIO . takeMVar . ncBlkSemaphore =<< getNodeContext

putBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => HeaderHash -> m ()
putBlkSemaphore tip = liftIO . flip putMVar tip . ncBlkSemaphore =<< getNodeContext

readBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => m HeaderHash
readBlkSemaphore = liftIO . readMVar . ncBlkSemaphore =<< getNodeContext

----------------------------------------------------------------------------
-- LRC synchronization
----------------------------------------------------------------------------

-- | Block until LRC data is available for given epoch.
waitLrc
    :: (MonadIO m, HasContext LrcContext m)
    => EpochIndex -> m ()
waitLrc epoch = do
    sync <- askContext @LrcContext lcLrcSync
    () <$ readTVarConditional ((>= epoch) . lastEpochWithLrc) sync

lrcActionOnEpoch
    :: (MonadIO m, HasContext LrcContext m, MonadThrow m)
    => EpochIndex
    -> (EpochIndex -> m (Maybe a))
    -> m a
lrcActionOnEpoch epoch =
    lrcActionOnEpochReason
        epoch
        "action on lrcCallOnEpoch couldn't be performed properly"

lrcActionOnEpochReason
    :: (MonadIO m, HasContext LrcContext m, MonadThrow m)
    => EpochIndex
    -> Text
    -> (EpochIndex -> m (Maybe a))
    -> m a
lrcActionOnEpochReason epoch reason actionDependsOnLrc = do
    waitLrc epoch
    actionDependsOnLrc epoch >>= maybeThrow (LrcDataUnknown epoch reason)

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

-- | Returns node uptime based on current time and 'ncStartTime'.
getUptime :: (MonadIO m, WithNodeContext ssc m) => m Microsecond
getUptime = do
    curTime <- liftIO getCurrentTime
    startTime <- ncStartTime <$> getNodeContext
    let seconds = toRational $ curTime `diffUTCTime` startTime
    pure $ fromMicroseconds $ round $ seconds * 1000 * 1000

-- | Returns if 'ncRecoveryHeader' is 'Just' which is equivalent to
-- "we're in recovery mode".
isRecoveryMode :: (MonadIO m, WithNodeContext ssc m) => m Bool
isRecoveryMode = do
    var <- ncRecoveryHeader <$> getNodeContext
    isJust <$> atomically (STM.tryReadTMVar var)
