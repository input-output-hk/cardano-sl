-- | In-memory state of LRC.

module Pos.Lrc.Context
       ( LrcSyncData(..)
       , LrcContext(..)
       , mkLrcSyncData

       , waitLrc
       , lrcActionOnEpoch
       , lrcActionOnEpochReason
       ) where

import           Universum

import qualified Ether

import           Pos.Core            (EpochIndex)
import           Pos.DB.Class        (MonadDBRead)
import           Pos.Lrc.DB.Common   (getEpoch)
import           Pos.Lrc.Error       (LrcError (..))
import           Pos.Util.Concurrent (readTVarConditional)
import           Pos.Util.Util       (maybeThrow)

data LrcContext = LrcContext
    { -- | Primitive for synchronization with LRC.
      lcLrcSync :: !(TVar LrcSyncData)
    }

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
    :: (MonadIO m, Ether.MonadReader' LrcContext m)
    => EpochIndex -> m ()
waitLrc epoch = do
    sync <- Ether.asks' @LrcContext lcLrcSync
    () <$ readTVarConditional ((>= epoch) . lastEpochWithLrc) sync

lrcActionOnEpoch
    :: (MonadIO m, Ether.MonadReader' LrcContext m, MonadThrow m)
    => EpochIndex
    -> (EpochIndex -> m (Maybe a))
    -> m a
lrcActionOnEpoch epoch =
    lrcActionOnEpochReason
        epoch
        "action on lrcCallOnEpoch couldn't be performed properly"

lrcActionOnEpochReason
    :: (MonadIO m, Ether.MonadReader' LrcContext m, MonadThrow m)
    => EpochIndex
    -> Text
    -> (EpochIndex -> m (Maybe a))
    -> m a
lrcActionOnEpochReason epoch reason actionDependsOnLrc = do
    waitLrc epoch
    actionDependsOnLrc epoch >>= maybeThrow (LrcDataUnknown epoch reason)
