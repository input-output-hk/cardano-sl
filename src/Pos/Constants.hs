-- | Several constants used by algorithm.

module Pos.Constants
       ( t
       , k
       , slotDuration
       , epochSlots
       ) where

import           Universum

import           Control.TimeWarp.Timed (Microsecond, sec)

-- |
t :: Integral a => a
t = 0

-- | Consensus guarantee (i.e. after what amount of blocks can we consider
-- blocks stable?).
k :: Integral a => a
k = 3

slotDuration :: Microsecond
slotDuration = sec 1

epochSlots :: Integral a => a
epochSlots = 6 * k
