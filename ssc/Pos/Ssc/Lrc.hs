module Pos.Ssc.Lrc
       ( sscLrcConsumer
       ) where

import           Pos.Core                 (BlockVersionData (bvdMpcThd))
import qualified Pos.DB                   as DB
import           Pos.Lrc.Consumer         (LrcConsumer (..),
                                           lrcConsumerFromComponentSimple)
import           Pos.Ssc.RichmenComponent (RCSsc)

-- | Consumer will be called on every Richmen computation.
sscLrcConsumer :: (DB.MonadGState m, DB.MonadDB m) => LrcConsumer m
sscLrcConsumer = lrcConsumerFromComponentSimple @RCSsc bvdMpcThd
