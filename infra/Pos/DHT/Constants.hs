module Pos.DHT.Constants
       ( enhancedMessageTimeout
       , neighborsSendThreshold
       , kademliaDumpInterval
       ) where

import           Universum

import           Pos.Infra.Constants (ccEnhancedMessageTimeout, ccKademliaDumpInterval,
                                      ccNeighboursSendThreshold, infraConstants)

-- | See 'Pos.CompileConfig.ccNeighboursSendThreshold'.
neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ infraConstants

-- | Interval for dumping state of Kademlia in slots
kademliaDumpInterval :: Integral a => a
kademliaDumpInterval = fromIntegral . ccKademliaDumpInterval $ infraConstants

-- | Broadcast to nodes which timestamp at most @enhancedMessageTimeout@.
enhancedMessageTimeout :: Integral a => a
enhancedMessageTimeout = fromIntegral . ccEnhancedMessageTimeout $ infraConstants
