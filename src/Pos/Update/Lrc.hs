-- | LRC part of Update System.

module Pos.Update.Lrc
       ( usLrcConsumer
       ) where

import qualified Pos.DB           as DB
import           Pos.DB.Lrc       (RCUs)
import           Pos.Lrc.Consumer (LrcConsumer (..), lrcConsumerFromComponentSimple)

-- | Consumer will be called on every Richmen computation.
usLrcConsumer :: DB.MonadDB ssc m => LrcConsumer m
usLrcConsumer = lrcConsumerFromComponentSimple @RCUs
