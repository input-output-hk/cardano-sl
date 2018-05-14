{-# LANGUAGE TypeFamilies #-}

-- | Richmen computation for delegation.

module Pos.Delegation.Lrc
       (
       -- * The 'RichmenComponent' instance
         RCDlg

       -- * The consumer
       , dlgLrcConsumer

       -- * Functions for getting richmen
       , getDlgRichmen
       , tryGetDlgRichmen
       ) where

import           Universum

import           Pos.Core (EpochIndex, bvdHeavyDelThd, genesisBlockVersionData, HasGenesisBlockVersionData)
import qualified Pos.DB as DB
import qualified Pos.Lrc.Consumer as Lrc
import qualified Pos.Lrc.Context as Lrc
import           Pos.Lrc.DB.RichmenBase as Lrc
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import qualified Pos.Lrc.Types as Lrc
import           Pos.Util.Util (getKeys)

----------------------------------------------------------------------------
-- RichmenComponent
----------------------------------------------------------------------------

data RCDlg

instance (HasGenesisBlockVersionData) => RichmenComponent RCDlg where
    type RichmenData RCDlg = Lrc.RichmenSet
    rcToData = getKeys . snd
    rcTag Proxy = "dlg"
    rcInitialThreshold Proxy = bvdHeavyDelThd genesisBlockVersionData
    rcConsiderDelegated Proxy = False

----------------------------------------------------------------------------
-- The consumer
----------------------------------------------------------------------------

-- | Consumer will be called on every Richmen computation.
dlgLrcConsumer :: (HasGenesisBlockVersionData, DB.MonadGState m, DB.MonadDB m) => Lrc.LrcConsumer m
dlgLrcConsumer = Lrc.lrcConsumerFromComponentSimple @RCDlg bvdHeavyDelThd

----------------------------------------------------------------------------
-- Getting richmen
----------------------------------------------------------------------------

-- | Wait for LRC results to become available and then get delegation ricmen
-- data for the given epoch.
getDlgRichmen
    :: (MonadIO m, DB.MonadDBRead m, MonadReader ctx m, Lrc.HasLrcContext ctx, HasGenesisBlockVersionData)
    => Text               -- ^ Function name (to include into error message)
    -> EpochIndex         -- ^ Epoch for which you want to know the richmen
    -> m Lrc.RichmenSet
getDlgRichmen fname epoch =
    Lrc.lrcActionOnEpochReason
        epoch
        (fname <> ": couldn't get delegation richmen")
        tryGetDlgRichmen

-- | Like 'getDlgRichmen', but doesn't wait and doesn't fail.
--
-- Returns a 'Maybe'.
tryGetDlgRichmen
    :: (HasGenesisBlockVersionData, DB.MonadDBRead m)
    => EpochIndex -> m (Maybe Lrc.RichmenSet)
tryGetDlgRichmen = getRichmen @RCDlg
