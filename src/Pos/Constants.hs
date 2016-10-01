-- | Several constants used by algorithm.

module Pos.Constants
       ( n
       , t
       , k
       , slotDuration
       , epochSlots
       ) where

import           Control.TimeWarp.Timed (Microsecond, sec)
import           Protolude              hiding (for, wait, (%))

n :: Integral a => a
n = 3

t :: Integral a => a
t = 0

k :: Integral a => a
k = 3

slotDuration :: Microsecond
slotDuration = sec 1

epochSlots :: Integral a => a
epochSlots = 6 * k
