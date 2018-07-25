{-# LANGUAGE TypeFamilies #-}

-- | Richmen computation for the update system.

module Pos.DB.Lrc.Consumer.Update
       (
       -- * The 'RichmenComponent' instance
         updateRichmenComponent

       -- * The consumer
       , usLrcConsumer

       -- * Functions for getting richmen
       , getUSRichmen
       , tryGetUSRichmen
       ) where

import           Universum

import           Pos.Core (EpochIndex, HasGenesisBlockVersionData,
                     bvdUpdateVoteThd, genesisBlockVersionData)
import           Pos.DB (MonadDB, MonadDBRead, MonadGState)
import           Pos.DB.Lrc.Consumer (LrcConsumer,
                     lrcConsumerFromComponentSimple)
import           Pos.DB.Lrc.Context (HasLrcContext, lrcActionOnEpochReason)
import           Pos.DB.Lrc.RichmenBase
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import           Pos.Lrc.Types (FullRichmenData)

----------------------------------------------------------------------------
-- RichmenComponent
----------------------------------------------------------------------------

updateRichmenComponent :: HasGenesisBlockVersionData => RichmenComponent FullRichmenData
updateRichmenComponent = RichmenComponent
    { rcToData            = identity
    , rcTag               = "us"
    , rcInitialThreshold  = bvdUpdateVoteThd genesisBlockVersionData
    , rcConsiderDelegated = True
    }

----------------------------------------------------------------------------
-- The consumer
----------------------------------------------------------------------------

-- | Consumer will be called on every Richmen computation.
usLrcConsumer :: (MonadGState m, MonadDB m) => LrcConsumer m
usLrcConsumer = lrcConsumerFromComponentSimple updateRichmenComponent bvdUpdateVoteThd

----------------------------------------------------------------------------
-- Getting richmen
----------------------------------------------------------------------------

-- | Wait for LRC results to become available and then get update system
-- ricmen data for the given epoch.
getUSRichmen
    :: (MonadIO m, MonadDBRead m, MonadReader ctx m, HasLrcContext ctx)
    => Text               -- ^ Function name (to include into error message)
    -> EpochIndex         -- ^ Epoch for which you want to know the richmen
    -> m FullRichmenData
getUSRichmen fname epoch = lrcActionOnEpochReason
    epoch
    (fname <> ": couldn't get US richmen")
    tryGetUSRichmen

-- | Like 'getUSRichmen', but doesn't wait and doesn't fail.
--
-- Returns a 'Maybe'.
tryGetUSRichmen :: MonadDBRead m => EpochIndex -> m (Maybe FullRichmenData)
tryGetUSRichmen = getRichmen updateRichmenComponent
