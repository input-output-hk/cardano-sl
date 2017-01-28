-- | General purpose workers for @SSC@ storage.

module Pos.Ssc.Class.Workers
       ( SscWorkersClass(..)
       ) where

import           Data.Tagged                (Tagged)
import           Pos.Communication.Protocol (Worker)



import           Pos.Lrc.Consumer           (LrcConsumer)
import           Pos.Ssc.Class.Types        (Ssc (..))
import           Pos.WorkMode               (WorkMode)

-- | Class for @SSC@ workers.
class Ssc ssc => SscWorkersClass ssc where
    -- | All workers specific to SSC.
    sscWorkers :: WorkMode ssc m => Tagged ssc [Worker m]
    sscLrcConsumers :: WorkMode ssc m => Tagged ssc [LrcConsumer m]
