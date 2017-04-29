module Pos.DHT.Constants
       ( enhancedMessageTimeout
       , neighborsSendThreshold
       , kademliaDumpInterval
       , enhancedMessageBroadcast
       ) where

import           Universum

import           Pos.Infra.Constants (ccEnhancedMessageBroadcast,
                                      ccEnhancedMessageTimeout, ccKademliaDumpInterval,
                                      ccNeighboursSendThreshold, infraConstants)

-- | See 'Pos.CompileConfig.ccNeighboursSendThreshold'.
neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ infraConstants

-- | Interval for dumping state of Kademlia in slots
kademliaDumpInterval :: Integral a => a
kademliaDumpInterval = fromIntegral . ccKademliaDumpInterval $ infraConstants

-- | Broadcast to nodes whose were pinged at most @enhancedMessageTimeout@ seconds ago.
enhancedMessageTimeout :: Integral a => a
enhancedMessageTimeout = fromIntegral . ccEnhancedMessageTimeout $ infraConstants

-- | Number of nodes from batch for enhanced bessage broadcast
enhancedMessageBroadcast :: Integral a => a
enhancedMessageBroadcast = fromIntegral . ccEnhancedMessageBroadcast $ infraConstants
