{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Pos.Lrc.Consumers
       (
         allLrcConsumers
       ) where

import           Pos.Delegation.Lrc    (delegationLrcConsumer)
import           Pos.Lrc.Consumer      (LrcConsumer)
import           Pos.Lrc.Mode          (LrcMode)
import           Pos.Ssc.Lrc           (sscLrcConsumer)
import           Pos.Ssc.GodTossing.Network.Constraint (GtMessageConstraints)
import           Pos.Update.Lrc        (usLrcConsumer)

allLrcConsumers
    :: forall ctx m. (GtMessageConstraints, LrcMode ctx m)
    => [LrcConsumer m]
allLrcConsumers = [delegationLrcConsumer, usLrcConsumer, sscLrcConsumer]
