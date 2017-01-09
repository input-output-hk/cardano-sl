-- | General purpose workers for @SSC@ storage.

module Pos.Ssc.Class.Workers
       ( SscWorkersClass(..)
       ) where

import           Data.Tagged           (Tagged)
import           Node                  (SendActions)

import           Pos.Communication.BiP (BiP)
import           Pos.Lrc.Types         (LrcConsumer)
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.WorkMode          (NewWorkMode)

-- | Class for @SSC@ workers.
class Ssc ssc => SscWorkersClass ssc where
    -- | All workers specific to SSC.
    sscWorkers :: NewWorkMode ssc m => Tagged ssc [SendActions BiP m -> m ()]
    sscLrcConsumers :: NewWorkMode ssc m => Tagged ssc [LrcConsumer m]
