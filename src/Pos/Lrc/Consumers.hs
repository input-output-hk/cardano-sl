module Pos.Lrc.Consumers
       (
         allLrcConsumers
       ) where

import           Universum

import           Data.Tagged           (untag)

import           Pos.Delegation.Lrc    (delegationLrcConsumer)
import           Pos.Lrc.Consumer      (LrcConsumer)
import           Pos.Lrc.Mode          (LrcMode)
import           Pos.Ssc.Class.Workers (SscWorkersClass (sscLrcConsumers))
import           Pos.Update.Lrc        (usLrcConsumer)

allLrcConsumers
    :: (LrcMode ssc m, SscWorkersClass ssc)
    => [LrcConsumer m]
allLrcConsumers = [delegationLrcConsumer, usLrcConsumer] ++ untag sscLrcConsumers
