module Pos.Lrc.Consumers
       (
         allLrcConsumers
       ) where

import           Data.Tagged           (untag)
import           Universum

import           Pos.Lrc.Consumer      (LrcConsumer)
import           Pos.Ssc.Class.Workers (SscWorkersClass (sscLrcConsumers))
import           Pos.WorkMode          (WorkMode)

allLrcConsumers
    :: (SscWorkersClass ssc, WorkMode ssc m)
    => [LrcConsumer m]
allLrcConsumers = concat $ [untag sscLrcConsumers]
