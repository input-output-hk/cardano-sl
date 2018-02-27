module Pos.DHT.Constants
       ( enhancedMessageTimeout
       , neighborsSendThreshold
       , kademliaDumpInterval
       , enhancedMessageBroadcast
       ) where

import           Universum

-- | Minimum number of neighbours to send to.
neighborsSendThreshold :: (Integral a) => a
neighborsSendThreshold = 2

-- | Interval for dumping state of Kademlia in slots
kademliaDumpInterval :: (Integral a) => a
kademliaDumpInterval = 4

-- | Broadcast to nodes whose were pinged at most @enhancedMessageTimeout@ seconds ago.
enhancedMessageTimeout :: (Integral a) => a
enhancedMessageTimeout = 360

-- | Number of nodes from batch for enhanced bessage broadcast
enhancedMessageBroadcast :: (Integral a) => a
enhancedMessageBroadcast = 2
