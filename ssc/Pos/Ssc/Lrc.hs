module Pos.Ssc.Lrc
       ( sscLrcConsumer
       , getSscRichmenFromLrc
       ) where

import           Universum

import           Pos.Core (BlockVersionData (bvdMpcThd), EpochIndex)
import qualified Pos.DB as DB
import qualified Pos.Lrc.Consumer as Lrc
import qualified Pos.Lrc.Context as Lrc
import qualified Pos.Lrc.Types as Lrc
import           Pos.Ssc.RichmenComponent (RCSsc, getRichmenSsc)

-- | Consumer will be called on every Richmen computation.
sscLrcConsumer :: (DB.MonadGState m, DB.MonadDB m) => Lrc.LrcConsumer m
sscLrcConsumer = Lrc.lrcConsumerFromComponentSimple @RCSsc bvdMpcThd

getSscRichmenFromLrc ::
       (MonadIO m, DB.MonadDBRead m, MonadReader ctx m, Lrc.HasLrcContext ctx)
    => Text
    -> EpochIndex
    -> m Lrc.RichmenStakes
getSscRichmenFromLrc fname epoch =
    Lrc.lrcActionOnEpochReason
        epoch
        (fname <> ": couldn't get SSC richmen")
        getRichmenSsc
