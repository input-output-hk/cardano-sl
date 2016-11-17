{-# LANGUAGE TemplateHaskell #-}

-- | Constants used by algorithm.
module Pos.Constants
       ( k
       , slotDuration
       , epochSlots
       , epochDuration
       , ftsSeedLength
       , genesisN
       , maxLocalTxs
       , sscTransmitterInterval
       , neighborsSendThreshold
       , networkDiameter
       , RunningMode (..)
       , runningMode
       , isDevelopment
       , defaultPeers
       ) where

import           Control.TimeWarp.Timed (Microsecond, sec)
import qualified Text.Parsec            as P
import           Universum

import           Pos.CLI                (dhtNodeParser)
import           Pos.CompileConfig      (CompileConfig (..), compileConfig)
import           Pos.DHT.Types          (DHTNode)
import           Pos.Types.Timestamp    (Timestamp)

-- | Consensus guarantee (i.e. after what amount of blocks can we consider
-- blocks stable?).
k :: Integral a => a
k = fromIntegral . ccK $ compileConfig

slotDuration :: Microsecond
slotDuration = sec . ccSlotDurationSec $ compileConfig

epochSlots :: Integral a => a
epochSlots = 6 * k

epochDuration :: Microsecond
epochDuration = epochSlots * slotDuration

ftsSeedLength :: Integral a => a
ftsSeedLength = 32

genesisN :: Integral i => i
genesisN = fromIntegral . ccGenesisN $ compileConfig

maxLocalTxs :: Integral i => i
maxLocalTxs = fromIntegral . ccMaxLocalTxs $ compileConfig

-- | Estimated time needed to broadcast message from one node to all
-- other nodes.
networkDiameter :: Microsecond
networkDiameter = sec . ccNetworkDiameter $ compileConfig

neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ compileConfig

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

sscTransmitterInterval :: Microsecond
sscTransmitterInterval = sec . ccMpcRelayInterval $ compileConfig

defaultPeers :: [DHTNode]
defaultPeers = map parsePeer . ccDefaultPeers $ compileConfig
  where
    parsePeer :: [Char] -> DHTNode
    parsePeer =
        either (panic . show) identity .
        P.parse dhtNodeParser "Compile time config"
