module Pos.Ssc.GodTossing.Richmen
       ( gtLrcConsumer
       ) where

-- import           Universum

import qualified Pos.DB           as DB
import           Pos.Lrc.Consumer (LrcConsumer (..), lrcConsumerFromComponentSimple)
import           Pos.Lrc.DB       (RCSsc)

-- | Consumer will be called on every Richmen computation.
gtLrcConsumer :: DB.MonadDB m => LrcConsumer m
gtLrcConsumer = lrcConsumerFromComponentSimple @RCSsc
