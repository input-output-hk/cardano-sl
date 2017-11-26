{-# LANGUAGE Rank2Types #-}

-- | Configuration used by the algorithm. See the paper for more details.

module Pos.Configuration
       ( NodeConfiguration (..)
       , HasNodeConfiguration
       , nodeConfiguration
       , withNodeConfiguration

       -- * Constants mentioned in paper
       , networkDiameter

       -- * Other constants
       , networkConnectionTimeout
       , conversationEstablishTimeout
       , blockRetrievalQueueSize
       , propagationQueueSize
       , defaultPeers
       , recoveryHeadersMessage

       -- * Malicious activity detection constants
       , mdNoBlocksSlotThreshold

       -- * Transaction resubmition constants
       , pendingTxResubmitionPeriod
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), genericParseJSON)
import           Data.Reflection (Given (..), give)
import           Data.Time.Units (Microsecond, Second)
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util (ms, sec)
import qualified Text.Parsec as P

import           Pos.Util.TimeWarp (NetworkAddress, addrParser)

type HasNodeConfiguration = Given NodeConfiguration

nodeConfiguration :: HasNodeConfiguration => NodeConfiguration
nodeConfiguration = given

withNodeConfiguration :: NodeConfiguration -> (HasNodeConfiguration => r) -> r
withNodeConfiguration = give

-- | Top-level node configuration. See example in /configuration.yaml/ file.
data NodeConfiguration = NodeConfiguration
    {
      ccNetworkDiameter              :: !Int
      -- ^ Estimated time for broadcasting messages
    , ccDefaultPeers                 :: ![Text]
      -- ^ List of default peers
    , ccMdNoBlocksSlotThreshold      :: !Int
      -- ^ Threshold of slots for malicious activity detection
    , ccRecoveryHeadersMessage       :: !Int
      -- ^ Numbers of headers put in message in recovery mode.
    , ccNetworkConnectionTimeout     :: !Int
      -- ^ Network connection timeout in milliseconds
    , ccConversationEstablishTimeout :: !Int
      -- ^ Conversation acknowledgement timeout in milliseconds.
      -- Default 30 seconds.
    , ccBlockRetrievalQueueSize      :: !Int
      -- ^ Block retrieval queue capacity
    , ccPropagationQueueSize         :: !Int
      -- ^ InvMsg propagation queue capacity
    , ccPendingTxResubmissionPeriod  :: !Int
      -- ^ Minimal delay between pending transactions resubmission
    } deriving (Show, Generic)

instance FromJSON NodeConfiguration where
    parseJSON = genericParseJSON defaultOptions

----------------------------------------------------------------------------
-- Main constants mentioned in paper
----------------------------------------------------------------------------

-- | Estimated time needed to broadcast message from one node to all
-- other nodes. Also see 'Pos.NodeConfiguration.ccNetworkDiameter'.
networkDiameter :: HasNodeConfiguration => Microsecond
networkDiameter = sec . ccNetworkDiameter $ nodeConfiguration

----------------------------------------------------------------------------
-- Other constants
----------------------------------------------------------------------------

networkConnectionTimeout :: HasNodeConfiguration => Microsecond
networkConnectionTimeout = ms . fromIntegral . ccNetworkConnectionTimeout $ nodeConfiguration

-- | Default is 30 seconds.
conversationEstablishTimeout :: HasNodeConfiguration => Microsecond
conversationEstablishTimeout = ms . fromIntegral . ccConversationEstablishTimeout $ nodeConfiguration

blockRetrievalQueueSize :: (HasNodeConfiguration, Integral a) => a
blockRetrievalQueueSize =
    fromIntegral . ccBlockRetrievalQueueSize $ nodeConfiguration

propagationQueueSize :: (HasNodeConfiguration, Integral a) => a
propagationQueueSize =
    fromIntegral $ ccPropagationQueueSize $ nodeConfiguration

-- | See 'Pos.NodeConfiguration.ccDefaultPeers'.
defaultPeers :: HasNodeConfiguration => [NetworkAddress]
defaultPeers = map parsePeer . ccDefaultPeers $ nodeConfiguration
  where
    parsePeer :: Text -> NetworkAddress
    parsePeer =
        either (error . show) identity .
        P.parse addrParser "Compile time config"

-- | Maximum amount of headers node can put into headers message while
-- in "after offline" or "recovery" mode. Should be more than
-- 'blkSecurityParam'.
recoveryHeadersMessage :: (HasNodeConfiguration, Integral a) => a
recoveryHeadersMessage = fromIntegral . ccRecoveryHeadersMessage $ nodeConfiguration

----------------------------------------------------------------------------
-- Malicious activity
----------------------------------------------------------------------------

-- | Number of slots used by malicious actions detection to check if
-- we are not receiving generated blocks.
mdNoBlocksSlotThreshold :: (HasNodeConfiguration, Integral i) => i
mdNoBlocksSlotThreshold = fromIntegral . ccMdNoBlocksSlotThreshold $ nodeConfiguration

----------------------------------------------------------------------------
-- Transactions resubmition
----------------------------------------------------------------------------

pendingTxResubmitionPeriod :: HasNodeConfiguration => Second
pendingTxResubmitionPeriod = fromIntegral . ccPendingTxResubmissionPeriod $ nodeConfiguration
