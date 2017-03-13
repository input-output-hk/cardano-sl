module Pos.DHT.Constants
       ( enhancedMessageBroadcast
       , neighborsSendThreshold
       , kademliaDumpInterval
       ) where

import           Universum

import           Pos.Infra.Constants (ccEnhancedMessageBroadcast, ccKademliaDumpInterval,
                                      ccNeighboursSendThreshold, infraConstants)

-- | See 'Pos.CompileConfig.ccNeighboursSendThreshold'.
neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ infraConstants

-- | Interval for dumping state of Kademlia in slots
kademliaDumpInterval :: Integral a => a
kademliaDumpInterval = fromIntegral . ccKademliaDumpInterval $ infraConstants

-- | Setting this to true enables enhanced message broadcast
enhancedMessageBroadcast :: Integral a => a
enhancedMessageBroadcast = fromIntegral . ccEnhancedMessageBroadcast $ infraConstants
