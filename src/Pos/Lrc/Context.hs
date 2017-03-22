module Pos.Lrc.Context
       ( LrcSyncData
       , LrcContext(..)
       ) where

import           Universum

import           Pos.Core  (EpochIndex)

data LrcContext = LrcContext
    {
    -- | Primitive for synchronization with LRC.
      lcLrcSync :: !(TVar LrcSyncData)
    }

-- | Data used for LRC syncronization. First value is __False__ iff
-- LRC is running now. Second value is last epoch for which we have
-- already computed LRC.
type LrcSyncData = (Bool, EpochIndex)
