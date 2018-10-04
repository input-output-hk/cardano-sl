{-# LANGUAGE TypeFamilies #-}

-- | Richmen computation for SSC.

module Pos.Lrc.Consumer.Ssc
       (
       -- * The 'RichmenComponent' instance
         richmenComponent

       -- * The consumer
       , sscLrcConsumer

       -- * Functions for getting richmen
       , getSscRichmen
       , tryGetSscRichmen
       ) where

import           Universum

import           Pos.Core (EpochIndex, HasGenesisBlockVersionData, bvdMpcThd,
                     genesisBlockVersionData)
import           Pos.DB (MonadDB, MonadDBRead, MonadGState)
import           Pos.Lrc.Consumer (LrcConsumer, lrcConsumerFromComponentSimple)
import           Pos.Lrc.Context (HasLrcContext, lrcActionOnEpochReason)
import           Pos.Lrc.DB.RichmenBase
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import           Pos.Lrc.Types (RichmenStakes)

----------------------------------------------------------------------------
-- RichmenComponent
----------------------------------------------------------------------------

richmenComponent :: HasGenesisBlockVersionData => RichmenComponent RichmenStakes
richmenComponent = RichmenComponent
    { rcToData            = snd
    , rcTag               = "ssc"
    , rcInitialThreshold  = bvdMpcThd genesisBlockVersionData
    , rcConsiderDelegated = True
    }

----------------------------------------------------------------------------
-- The consumer
----------------------------------------------------------------------------

-- | Consumer will be called on every Richmen computation.
sscLrcConsumer :: (MonadGState m, MonadDB m) => LrcConsumer m
sscLrcConsumer = lrcConsumerFromComponentSimple richmenComponent bvdMpcThd

----------------------------------------------------------------------------
-- Getting richmen
----------------------------------------------------------------------------

-- | Wait for LRC results to become available and then get the list of SSC
-- ricmen for the given epoch.
getSscRichmen
    :: (MonadIO m, MonadDBRead m, MonadReader ctx m, HasLrcContext ctx)
    => Text               -- ^ Function name (to include into error message)
    -> EpochIndex         -- ^ Epoch for which you want to know the richmen
    -> m RichmenStakes
getSscRichmen fname epoch = lrcActionOnEpochReason
    epoch
    (fname <> ": couldn't get SSC richmen")
    tryGetSscRichmen

-- | Like 'getSscRichmen', but doesn't wait and doesn't fail.
tryGetSscRichmen :: MonadDBRead m => EpochIndex -> m (Maybe RichmenStakes)
tryGetSscRichmen = getRichmen richmenComponent
