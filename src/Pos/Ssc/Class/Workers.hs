module Pos.Ssc.Class.Workers
       ( SscWorkersClass(..)
       ) where

import           Data.Tagged         (Tagged)

import           Pos.Ssc.Class.Types (SscTypes (..))
import           Pos.Types.Types     (SlotId)
import           Pos.WorkMode        (WorkMode)

class SscTypes ssc => SscWorkersClass ssc where
    -- | Action which should be done when new slot starts.
    sscOnNewSlot :: WorkMode ssc m => Tagged ssc (SlotId -> m ())
    -- | All workers specific to SSC.
    -- Exceptions:
    -- 1. Worker which ticks when new slot starts.
    sscWorkers :: WorkMode ssc m => Tagged ssc [m ()]
