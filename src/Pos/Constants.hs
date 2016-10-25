-- | Several constants used by algorithm.

module Pos.Constants
       ( k
       , slotDuration
       , epochSlots
       , epochDuration
       , neighborsSendThreshold
       , networkDiameter
       ) where

import           Universum

import           Control.TimeWarp.Timed (Microsecond, sec)

-- | Consensus guarantee (i.e. after what amount of blocks can we consider
-- blocks stable?).
k :: Integral a => a
k = 2

slotDuration :: Microsecond
slotDuration = sec 4

epochSlots :: Integral a => a
epochSlots = 6 * k

epochDuration :: Microsecond
epochDuration = epochSlots * slotDuration

-- | Estimated time needed to broadcast message from one node to all
-- other nodes.
networkDiameter :: Microsecond
networkDiameter = sec 1

neighborsSendThreshold :: Int
neighborsSendThreshold = 6
