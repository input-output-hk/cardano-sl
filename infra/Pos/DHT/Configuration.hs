module Pos.DHT.Configuration
       ( enhancedMessageTimeout
       , neighborsSendThreshold
       , kademliaDumpInterval
       , enhancedMessageBroadcast
       ) where

import           Universum

import           Pos.Infra.Configuration (HasInfraConfiguration, ccEnhancedMessageBroadcast,
                                          ccEnhancedMessageTimeout, ccKademliaDumpInterval,
                                          ccNeighboursSendThreshold, infraConfiguration)

-- | See 'Pos.CompileConfig.ccNeighboursSendThreshold'.
neighborsSendThreshold :: (HasInfraConfiguration, Integral a) => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ infraConfiguration

-- | Interval for dumping state of Kademlia in slots
kademliaDumpInterval :: (HasInfraConfiguration, Integral a) => a
kademliaDumpInterval = fromIntegral . ccKademliaDumpInterval $ infraConfiguration

-- | Broadcast to nodes whose were pinged at most @enhancedMessageTimeout@ seconds ago.
enhancedMessageTimeout :: (HasInfraConfiguration, Integral a) => a
enhancedMessageTimeout = fromIntegral . ccEnhancedMessageTimeout $ infraConfiguration

-- | Number of nodes from batch for enhanced bessage broadcast
enhancedMessageBroadcast :: (HasInfraConfiguration, Integral a) => a
enhancedMessageBroadcast = fromIntegral . ccEnhancedMessageBroadcast $ infraConfiguration
