-- | In-memory state of LRC.

module Pos.Lrc.Context
       ( LrcSyncData(..)
       , LrcContext(..)
       , HasLrcContext
       , cloneLrcContext
       , mkLrcSyncData

       , waitLrc
       , lrcActionOnEpoch
       , lrcActionOnEpochReason
       ) where

import           Universum

import           Control.Lens (views)
import           System.Wlog (WithLogger)

import           Pos.Core (EpochIndex)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Exception (reportFatalError)
import           Pos.Lrc.DB.Common (getEpoch)
import           Pos.Lrc.Error (LrcError (..))
import           Pos.Util.Concurrent (readTVarConditional)
import           Pos.Util.Util (HasLens (..), HasLens', maybeThrow)

data LrcContext = LrcContext
    { -- | Primitive for synchronization with LRC.
      lcLrcSync :: !(TVar LrcSyncData)
    }

type HasLrcContext ctx = HasLens' ctx LrcContext

-- | Create a new 'LrcContext' with the same contents as the given
-- context has.
cloneLrcContext ::
       (WithLogger m, MonadIO m, MonadThrow m) => LrcContext -> m LrcContext
cloneLrcContext LrcContext {..} = do
    readTVarIO lcLrcSync >>= \case
        lsd@LrcSyncData {..}
            | lrcNotRunning -> LrcContext <$> newTVarIO lsd
            | otherwise ->
                reportFatalError
                    "Someone tried to clone 'LrcContext' while LRC is running. It's weird"

-- | Data used for LRC syncronization. First value is __False__ iff
-- LRC is running now. Second value is last epoch for which we have
-- already computed LRC.
data LrcSyncData = LrcSyncData
    { lrcNotRunning    :: !Bool
    , lastEpochWithLrc :: !EpochIndex
    }

-- | Make new 'LrcSyncData' using read-only access to DB.
mkLrcSyncData :: MonadDBRead m => m LrcSyncData
mkLrcSyncData = LrcSyncData True <$> getEpoch

----------------------------------------------------------------------------
-- LRC synchronization
----------------------------------------------------------------------------

-- | Block until LRC data is available for given epoch.
waitLrc
    :: (MonadIO m, MonadReader ctx m, HasLrcContext ctx)
    => EpochIndex -> m ()
waitLrc epoch = do
    sync <- views (lensOf @LrcContext) lcLrcSync
    () <$ readTVarConditional ((>= epoch) . lastEpochWithLrc) sync

lrcActionOnEpoch
    :: (MonadIO m, MonadReader ctx m, HasLrcContext ctx, MonadThrow m)
    => EpochIndex
    -> (EpochIndex -> m (Maybe a))
    -> m a
lrcActionOnEpoch epoch =
    lrcActionOnEpochReason
        epoch
        "action on lrcActionOnEpoch couldn't be performed properly"

lrcActionOnEpochReason
    :: (MonadIO m, MonadReader ctx m, HasLrcContext ctx, MonadThrow m)
    => EpochIndex
    -> Text
    -> (EpochIndex -> m (Maybe a))
    -> m a
lrcActionOnEpochReason epoch reason actionDependsOnLrc = do
    waitLrc epoch
    actionDependsOnLrc epoch >>= maybeThrow (LrcDataUnknown epoch reason)
