{-# LANGUAGE TypeFamilies #-}

-- | Richmen computation for the update system.

module Pos.Update.Lrc
       (
       -- * The 'RichmenComponent' instance
         RCUs

       -- * The consumer
       , usLrcConsumer

       -- * Functions for getting richmen
       , getUSRichmen
       , tryGetUSRichmen
       ) where

import           Universum

import           Pos.Core (EpochIndex, HasGenesisBlockVersionData, bvdUpdateVoteThd,
                           genesisBlockVersionData)
import qualified Pos.DB as DB
import qualified Pos.Lrc.Consumer as Lrc
import qualified Pos.Lrc.Context as Lrc
import           Pos.Lrc.DB.RichmenBase as Lrc
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import qualified Pos.Lrc.Types as Lrc

----------------------------------------------------------------------------
-- RichmenComponent
----------------------------------------------------------------------------

-- | A tag for the update system 'RichmenComponent'
data RCUs

instance HasGenesisBlockVersionData => RichmenComponent RCUs where
    type RichmenData RCUs = Lrc.FullRichmenData
    rcToData = identity
    rcTag Proxy = "us"
    rcInitialThreshold Proxy = bvdUpdateVoteThd genesisBlockVersionData
    rcConsiderDelegated Proxy = True

----------------------------------------------------------------------------
-- The consumer
----------------------------------------------------------------------------

-- | Consumer will be called on every Richmen computation.
usLrcConsumer :: (HasGenesisBlockVersionData, DB.MonadGState m, DB.MonadDB m) => Lrc.LrcConsumer m
usLrcConsumer = Lrc.lrcConsumerFromComponentSimple @RCUs bvdUpdateVoteThd

----------------------------------------------------------------------------
-- Getting richmen
----------------------------------------------------------------------------

-- | Wait for LRC results to become available and then get update system
-- ricmen data for the given epoch.
getUSRichmen
    :: (MonadIO m, DB.MonadDBRead m, MonadReader ctx m, Lrc.HasLrcContext ctx, HasGenesisBlockVersionData)
    => Text               -- ^ Function name (to include into error message)
    -> EpochIndex         -- ^ Epoch for which you want to know the richmen
    -> m Lrc.FullRichmenData
getUSRichmen fname epoch =
    Lrc.lrcActionOnEpochReason
        epoch
        (fname <> ": couldn't get US richmen")
        tryGetUSRichmen

-- | Like 'getUSRichmen', but doesn't wait and doesn't fail.
--
-- Returns a 'Maybe'.
tryGetUSRichmen
    :: (DB.MonadDBRead m, HasGenesisBlockVersionData)
    => EpochIndex -> m (Maybe Lrc.FullRichmenData)
tryGetUSRichmen = getRichmen @RCUs
