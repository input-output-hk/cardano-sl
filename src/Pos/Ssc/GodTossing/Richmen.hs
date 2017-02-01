module Pos.Ssc.GodTossing.Richmen
       ( gtLrcConsumer
       ) where

-- import           Universum

import qualified Pos.DB                  as DB
import           Pos.DB.Lrc              (RCSsc)
import           Pos.Lrc.Consumer        (LrcConsumer (..),
                                          lrcConsumerFromComponentSimple)
import           Pos.Ssc.GodTossing.Type (SscGodTossing)

-- | Consumer will be called on every Richmen computation.
gtLrcConsumer :: DB.MonadDB SscGodTossing m => LrcConsumer m
gtLrcConsumer = lrcConsumerFromComponentSimple @RCSsc
