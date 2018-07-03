{-# LANGUAGE TypeFamilies #-}

-- | Richmen computation for delegation.

module Pos.Lrc.Consumer.Delegation
       (
       -- * The 'RichmenComponent'
         richmenComponent

       -- * The consumer
       , dlgLrcConsumer

       -- * Functions for getting richmen
       , getDlgRichmen
       , tryGetDlgRichmen
       ) where

import           Universum

import           Pos.Core (EpochIndex, HasGenesisBlockVersionData,
                     bvdHeavyDelThd, genesisBlockVersionData)
import           Pos.DB (MonadDB, MonadDBRead, MonadGState)
import           Pos.Lrc.Consumer (LrcConsumer, lrcConsumerFromComponentSimple)
import           Pos.Lrc.Context (HasLrcContext, lrcActionOnEpochReason)
import           Pos.Lrc.DB.RichmenBase
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import           Pos.Lrc.Types (RichmenSet)
import           Pos.Util.Util (getKeys)

----------------------------------------------------------------------------
-- RichmenComponent
----------------------------------------------------------------------------

richmenComponent :: HasGenesisBlockVersionData => RichmenComponent RichmenSet
richmenComponent = RichmenComponent
    { rcToData            = getKeys . snd
    , rcTag               = "dlg"
    , rcInitialThreshold  = bvdHeavyDelThd genesisBlockVersionData
    , rcConsiderDelegated = False
    }

----------------------------------------------------------------------------
-- The consumer
----------------------------------------------------------------------------

-- | Consumer will be called on every Richmen computation.
dlgLrcConsumer :: (MonadGState m, MonadDB m) => LrcConsumer m
dlgLrcConsumer = lrcConsumerFromComponentSimple richmenComponent bvdHeavyDelThd

----------------------------------------------------------------------------
-- Getting richmen
----------------------------------------------------------------------------

-- | Wait for LRC results to become available and then get delegation ricmen
-- data for the given epoch.
getDlgRichmen
    :: (MonadIO m, MonadDBRead m, MonadReader ctx m, HasLrcContext ctx)
    => Text               -- ^ Function name (to include into error message)
    -> EpochIndex         -- ^ Epoch for which you want to know the richmen
    -> m RichmenSet
getDlgRichmen fname epoch = lrcActionOnEpochReason
    epoch
    (fname <> ": couldn't get delegation richmen")
    tryGetDlgRichmen

-- | Like 'getDlgRichmen', but doesn't wait and doesn't fail.
tryGetDlgRichmen :: MonadDBRead m => EpochIndex -> m (Maybe RichmenSet)
tryGetDlgRichmen = getRichmen richmenComponent
