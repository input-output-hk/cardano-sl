{-# LANGUAGE Rank2Types #-}

-- | Configuration used by the algorithm. See the paper for more details.

module Pos.Configuration
       ( NodeConfiguration (..)
       , HasNodeConfiguration
       , nodeConfiguration
       , withNodeConfiguration

       -- * Other constants
       , networkConnectionTimeout
       , networkConnectionTimeouts
       , conversationEstablishTimeout
       , blockRetrievalQueueSize
       , propagationQueueSize
       , defaultPeers

       -- * Wallet constants
       , pendingTxResubmitionPeriod
       , walletProductionApi
       , walletTxCreationDisabled
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), genericParseJSON)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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
    , ccNetworkConnectionTimeouts    :: !(NonEmpty Int)
      -- ^ Network connection timeouts in milliseconds.  It must be an
      -- increasing sequence.  If a timeout will happen to be to small the next
      -- one will be used.
    , ccConversationEstablishTimeout :: !Int
      -- ^ Conversation acknowledgement timeout in milliseconds.
      -- Default 30 seconds.
    , ccBlockRetrievalQueueSize      :: !Int
      -- ^ Block retrieval queue capacity
    , ccPropagationQueueSize         :: !Int
      -- ^ InvMsg propagation queue capacity
    , ccPendingTxResubmissionPeriod  :: !Int
      -- ^ Minimal delay between pending transactions resubmission
    , ccWalletProductionApi          :: !Bool
      -- ^ Whether hazard wallet endpoint should be disabled
    , ccWalletTxCreationDisabled     :: !Bool
      -- ^ Disallow transaction creation or re-submission of
      -- pending transactions by the wallet
    } deriving (Show, Generic)

instance FromJSON NodeConfiguration where
    parseJSON = genericParseJSON defaultOptions

----------------------------------------------------------------------------
-- Miscellaneous constants
----------------------------------------------------------------------------

networkConnectionTimeouts :: HasNodeConfiguration => NonEmpty Microsecond
networkConnectionTimeouts = map (ms . fromIntegral) . ccNetworkConnectionTimeouts $ nodeConfiguration

networkConnectionTimeout :: HasNodeConfiguration => Microsecond
networkConnectionTimeout = NE.head networkConnectionTimeouts

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
-- Wallet parameters
----------------------------------------------------------------------------

pendingTxResubmitionPeriod :: HasNodeConfiguration => Second
pendingTxResubmitionPeriod = fromIntegral . ccPendingTxResubmissionPeriod $ nodeConfiguration

-- | If 'True', some dangerous endpoints, like one which resets wallet state,
-- should throw exception if used.
walletProductionApi :: HasNodeConfiguration => Bool
walletProductionApi = ccWalletProductionApi $ nodeConfiguration

-- | If 'True', wallet should *not* create new transactions or re-submit
-- existing pending transactions.
walletTxCreationDisabled :: HasNodeConfiguration => Bool
walletTxCreationDisabled = ccWalletTxCreationDisabled $ nodeConfiguration
