{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Pos.Lrc.Consumers
       (
         allLrcConsumers
       ) where

import           Universum

import           Pos.Delegation.Lrc    (delegationLrcConsumer)
import           Pos.Lrc.Consumer      (LrcConsumer)
import           Pos.Lrc.Mode          (LrcMode)
import           Pos.Ssc.Class.Workers (SscWorkersClass (sscLrcConsumers))
import           Pos.Update.Lrc        (usLrcConsumer)
import           Pos.Ssc.GodTossing.Type (SscGodTossing)

allLrcConsumers
    :: forall ctx m.
       (LrcMode ctx m, SscWorkersClass SscGodTossing)
    => [LrcConsumer m]
allLrcConsumers = [delegationLrcConsumer, usLrcConsumer] ++
                  sscLrcConsumers @SscGodTossing
