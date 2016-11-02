{-# LANGUAGE TemplateHaskell #-}

-- | Constants used by algorithm.
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

import           Control.TimeWarp.Timed (Microsecond, sec)
import           Universum

import           Pos.CompileConfig      (CompileConfig (..), compileConfig)
import           Pos.Types.Timestamp    (Timestamp)

-- | Consensus guarantee (i.e. after what amount of blocks can we consider
-- blocks stable?).
k :: Integral a => a
k = fromInteger . toInteger . ccK $ compileConfig

slotDuration :: Microsecond
slotDuration = sec . ccSlotDurationSec $ compileConfig

epochSlots :: Integral a => a
epochSlots = 6 * k

epochDuration :: Microsecond
epochDuration = epochSlots * slotDuration

-- | Estimated time needed to broadcast message from one node to all
-- other nodes.
networkDiameter :: Microsecond
networkDiameter = sec . ccNetworkDiameter $ compileConfig

neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromInteger . toInteger . ccNeighboursSendThreshold $ compileConfig

data RunningMode
    = Development
    | Production { rmSystemStart :: !Timestamp}

-- TODO switch between Development/Production via `stack` flag
runningMode :: RunningMode
runningMode = Development

isDevelopment :: Bool
isDevelopment = case runningMode of
                  Development -> True
                  _           -> False
