-- | Types used in LRC DB (temporary module).

module Pos.DB.Lrc.Types
       ( LeadersStorage (..)
       , GtRichmenStorage (..)
       ) where

import           Universum

import           Pos.Lrc.Types (RichmenStake)
import           Pos.Types     (EpochIndex, SlotLeaders)

----------------------------------------------------------------------------
-- LRC
----------------------------------------------------------------------------

data LeadersStorage ssc = LeadersStorage
    { lrcEpoch   :: !EpochIndex
    , lrcLeaders :: !SlotLeaders
    } deriving (Generic)

data GtRichmenStorage ssc = GtRichmenStorage
    { gtRichmenEpoch :: !EpochIndex
    , gtRichmen      :: !RichmenStake
    } deriving (Generic)
