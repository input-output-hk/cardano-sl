-- | Several constants used by algorithm.

module Pos.Constants
       ( k
       , slotDuration
       , epochSlots
       , epochDuration
       , neighborsSendThreshold
       , networkDiameter
       , RunningMode (..)
       , runningMode
       , isDevelopment
       ) where

import           Universum

import           Control.TimeWarp.Timed (Microsecond, sec)
import           Pos.Types.Timestamp    (Timestamp)

-- | Consensus guarantee (i.e. after what amount of blocks can we consider
-- blocks stable?).
k :: Integral a => a
k = 2

slotDuration :: Microsecond
slotDuration = sec 15

epochSlots :: Integral a => a
epochSlots = 6 * k

epochDuration :: Microsecond
epochDuration = epochSlots * slotDuration

-- | Estimated time needed to broadcast message from one node to all
-- other nodes.
networkDiameter :: Microsecond
networkDiameter = sec 3

neighborsSendThreshold :: Integral a => a
neighborsSendThreshold = 4


data RunningMode = Development
                 | Production
                    { rmSystemStart :: !Timestamp
                    }

-- TODO switch between Development/Production via `stack` flag
runningMode :: RunningMode
runningMode = Development

isDevelopment :: Bool
isDevelopment = case runningMode of
                  Development -> True
                  _           -> False
