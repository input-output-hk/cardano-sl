module Pos.Ssc.GodTossing.Richmen
       ( gtLrcConsumer
       ) where

import           Pos.Core                 (BlockVersionData (bvdMpcThd))
import qualified Pos.DB                   as DB
import           Pos.Lrc.Consumer         (LrcConsumer (..),
                                           lrcConsumerFromComponentSimple)
import           Pos.Ssc.RichmenComponent (RCSsc)

-- | Consumer will be called on every Richmen computation.
gtLrcConsumer :: DB.MonadDBCore m => LrcConsumer m
gtLrcConsumer = lrcConsumerFromComponentSimple @RCSsc bvdMpcThd
