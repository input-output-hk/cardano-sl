{-# LANGUAGE Rank2Types #-}

-- | Configuration used by the algorithm. See the paper for more details.

module Pos.Configuration
       ( NodeConfiguration (..)
       , HasNodeConfiguration
       , nodeConfiguration
       , withNodeConfiguration

       -- * Other constants
       , networkConnectionTimeout
       , conversationEstablishTimeout
       , blockRetrievalQueueSize
       , propagationQueueSize
       , defaultPeers

       -- * Transaction resubmition constants
       , pendingTxResubmitionPeriod
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), genericParseJSON)
import           Data.Reflection (Given (..), give)
import           Data.Time.Units (Microsecond, Second)
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util (ms)
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
      ccDefaultPeers                 :: ![Text]
      -- ^ List of default peers
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
-- Miscellaneous constants
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


----------------------------------------------------------------------------
-- Transactions resubmition
----------------------------------------------------------------------------

pendingTxResubmitionPeriod :: HasNodeConfiguration => Second
pendingTxResubmitionPeriod = fromIntegral . ccPendingTxResubmissionPeriod $ nodeConfiguration
