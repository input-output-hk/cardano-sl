-- | LRC part of Update System.

module Pos.Update.Lrc
       ( usLrcConsumer
       ) where

import           Pos.Core         (BlockVersionData (bvdUpdateVoteThd))
import qualified Pos.DB           as DB
import           Pos.Lrc.Consumer (LrcConsumer (..), lrcConsumerFromComponentSimple)
import           Pos.Lrc.DB       (RCUs)

-- | Consumer will be called on every Richmen computation.
usLrcConsumer :: DB.MonadRealDBCore m => LrcConsumer m
usLrcConsumer = lrcConsumerFromComponentSimple @RCUs bvdUpdateVoteThd
