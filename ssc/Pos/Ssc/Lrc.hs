{-# LANGUAGE TypeFamilies #-}

-- | Richmen computation for SSC.

module Pos.Ssc.Lrc
       (
       -- * The 'RichmenComponent' instance
         RCSsc

       -- * The consumer
       , sscLrcConsumer

       -- * Functions for getting richmen
       , getSscRichmen
       , tryGetSscRichmen
       ) where

import           Universum

import           Pos.Core (EpochIndex, HasGenesisBlockVersionData, bvdMpcThd,
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

-- | A tag for the SSC 'RichmenComponent'
data RCSsc

instance HasGenesisBlockVersionData => RichmenComponent RCSsc where
    type RichmenData RCSsc = Lrc.RichmenStakes
    rcToData = snd
    rcTag Proxy = "ssc"
    rcInitialThreshold Proxy = bvdMpcThd genesisBlockVersionData
    rcConsiderDelegated Proxy = True

----------------------------------------------------------------------------
-- The consumer
----------------------------------------------------------------------------

-- | Consumer will be called on every Richmen computation.
sscLrcConsumer :: (DB.MonadGState m, DB.MonadDB m, HasGenesisBlockVersionData) => Lrc.LrcConsumer m
sscLrcConsumer = Lrc.lrcConsumerFromComponentSimple @RCSsc bvdMpcThd

----------------------------------------------------------------------------
-- Getting richmen
----------------------------------------------------------------------------

-- | Wait for LRC results to become available and then get the list of SSC
-- ricmen for the given epoch.
getSscRichmen
    :: (MonadIO m, DB.MonadDBRead m, MonadReader ctx m, Lrc.HasLrcContext ctx, HasGenesisBlockVersionData)
    => Text               -- ^ Function name (to include into error message)
    -> EpochIndex         -- ^ Epoch for which you want to know the richmen
    -> m Lrc.RichmenStakes
getSscRichmen fname epoch =
    Lrc.lrcActionOnEpochReason
        epoch
        (fname <> ": couldn't get SSC richmen")
        tryGetSscRichmen

-- | Like 'getSscRichmen', but doesn't wait and doesn't fail.
--
-- Returns a 'Maybe'.
tryGetSscRichmen
    :: (DB.MonadDBRead m, HasGenesisBlockVersionData)
    => EpochIndex -> m (Maybe Lrc.RichmenStakes)
tryGetSscRichmen = Lrc.getRichmen @RCSsc
