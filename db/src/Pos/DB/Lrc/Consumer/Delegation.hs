{-# LANGUAGE TypeFamilies #-}

-- | Richmen computation for delegation.

module Pos.DB.Lrc.Consumer.Delegation
       (
       -- * The 'RichmenComponent'
         dlgRichmenComponent

       -- * The consumer
       , dlgLrcConsumer

       -- * Functions for getting richmen
       , getDlgRichmen
       , tryGetDlgRichmen
       , getDlgRichmenObft
       ) where

import           Universum

import           Data.HashSet (fromList)

import           Pos.Chain.Genesis (configGenesisWStakeholders)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Lrc (RichmenComponent (..), RichmenSet)
import           Pos.Chain.Update (BlockVersionData (..))
import           Pos.Core (EpochIndex)
import           Pos.DB (MonadDB, MonadDBRead, MonadGState)
import           Pos.DB.Lrc.Consumer (LrcConsumer,
                     lrcConsumerFromComponentSimple)
import           Pos.DB.Lrc.Context (HasLrcContext, lrcActionOnEpochReason)
import           Pos.DB.Lrc.RichmenBase
import           Pos.Util.Util (getKeys)

----------------------------------------------------------------------------
-- RichmenComponent
----------------------------------------------------------------------------

dlgRichmenComponent :: BlockVersionData -> RichmenComponent RichmenSet
dlgRichmenComponent genesisBvd = RichmenComponent
    { rcToData            = getKeys . snd
    , rcTag               = "dlg"
    , rcInitialThreshold  = bvdHeavyDelThd genesisBvd
    , rcConsiderDelegated = False
    }

----------------------------------------------------------------------------
-- The consumer
----------------------------------------------------------------------------

-- | Consumer will be called on every Richmen computation.
dlgLrcConsumer :: (MonadGState m, MonadDB m) => BlockVersionData -> LrcConsumer m
dlgLrcConsumer genesisBvd = lrcConsumerFromComponentSimple
    (dlgRichmenComponent genesisBvd)
    bvdHeavyDelThd

----------------------------------------------------------------------------
-- Getting richmen
----------------------------------------------------------------------------

-- | Wait for LRC results to become available and then get delegation ricmen
-- data for the given epoch.
getDlgRichmen
    :: (MonadIO m, MonadDBRead m, MonadReader ctx m, HasLrcContext ctx)
    => BlockVersionData
    -> Text               -- ^ Function name (to include into error message)
    -> EpochIndex         -- ^ Epoch for which you want to know the richmen
    -> m RichmenSet
getDlgRichmen genesisBvd fname epoch = lrcActionOnEpochReason
    epoch
    (fname <> ": couldn't get delegation richmen")
    (tryGetDlgRichmen genesisBvd)

-- | Like 'getDlgRichmen', but doesn't wait and doesn't fail.
tryGetDlgRichmen
    :: MonadDBRead m => BlockVersionData -> EpochIndex -> m (Maybe RichmenSet)
tryGetDlgRichmen = getRichmen . dlgRichmenComponent

-- | For OBFT, we retrieve the genesis stakeholders and classify them as the
-- "richmen". We don't perform any LRC operations here.
getDlgRichmenObft
    :: Genesis.Config
    -> RichmenSet
getDlgRichmenObft = fromList . configGenesisWStakeholders
