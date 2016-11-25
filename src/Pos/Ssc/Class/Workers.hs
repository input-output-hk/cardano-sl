{-# LANGUAGE FlexibleContexts #-}
-- | General purpose workers for @SSC@ storage.

module Pos.Ssc.Class.Workers
       ( SscWorkersClass(..)
       ) where

import           Data.Tagged         (Tagged)

import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Types.Types     (SlotId)
import           Pos.WorkMode        (WorkMode)

-- | Class for @SSC@ workers.
class Ssc ssc => SscWorkersClass ssc where
    -- | All workers specific to SSC.
    -- Exceptions:
    -- 1. Worker which ticks when new slot starts.
    sscWorkers :: WorkMode ssc m => Tagged ssc [m ()]
