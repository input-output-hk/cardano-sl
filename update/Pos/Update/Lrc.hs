-- | LRC part of Update System.

module Pos.Update.Lrc
       ( usLrcConsumer
       ) where

import           Pos.Core                    (BlockVersionData (bvdUpdateVoteThd))
import           Pos.DB.Class                (MonadDB, MonadGState)
import           Pos.Lrc.Consumer            (LrcConsumer (..),
                                              lrcConsumerFromComponentSimple)
import           Pos.Update.RichmenComponent (RCUs)

-- | Consumer will be called on every Richmen computation.
usLrcConsumer :: (MonadGState m, MonadDB m) => LrcConsumer m
usLrcConsumer = lrcConsumerFromComponentSimple @RCUs bvdUpdateVoteThd
