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

       -- * Wallet constants
       , pendingTxResubmitionPeriod
       , walletProductionApi
       , walletTxCreationDisabled

       -- * Explorer constants
       , explorerExtendedApi
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON,
                     genericToJSON)
import           Data.Aeson.Options (defaultOptions)
import           Data.Reflection (Given (..), give)
import           Data.Time.Units (Microsecond, Second, fromMicroseconds)

type HasNodeConfiguration = Given NodeConfiguration

nodeConfiguration :: HasNodeConfiguration => NodeConfiguration
nodeConfiguration = given

withNodeConfiguration :: NodeConfiguration -> (HasNodeConfiguration => r) -> r
withNodeConfiguration = give

-- | Top-level node configuration. See example in /configuration.yaml/ file.
data NodeConfiguration = NodeConfiguration
    {
      ccNetworkConnectionTimeout     :: !Int
      -- ^ Network connection timeout in milliseconds
    , ccConversationEstablishTimeout :: !Int
      -- ^ Conversation acknowledgement timeout in milliseconds.
      -- Default 30 seconds.
    , ccBlockRetrievalQueueSize      :: !Int
      -- ^ Block retrieval queue capacity
    , ccPendingTxResubmissionPeriod  :: !Int
      -- ^ Minimal delay between pending transactions resubmission
    , ccWalletProductionApi          :: !Bool
      -- ^ Whether hazard wallet endpoint should be disabled
    , ccWalletTxCreationDisabled     :: !Bool
      -- ^ Disallow transaction creation or re-submission of
      -- pending transactions by the wallet
    , ccExplorerExtendedApi          :: !Bool
      -- ^ Enable explorer extended API for fetching more
      -- info about addresses (like utxos) and bulk endpoints
    } deriving (Show, Generic)

instance ToJSON NodeConfiguration where
    toJSON = genericToJSON defaultOptions

instance FromJSON NodeConfiguration where
    parseJSON = genericParseJSON defaultOptions

----------------------------------------------------------------------------
-- Miscellaneous constants
----------------------------------------------------------------------------

networkConnectionTimeout :: HasNodeConfiguration => Microsecond
networkConnectionTimeout = fromMicroseconds . (*) 1000 . fromIntegral . ccNetworkConnectionTimeout $ nodeConfiguration

-- | Default is 30 seconds.
conversationEstablishTimeout :: HasNodeConfiguration => Microsecond
conversationEstablishTimeout = fromMicroseconds . (*) 1000 . fromIntegral . ccConversationEstablishTimeout $ nodeConfiguration

blockRetrievalQueueSize :: (HasNodeConfiguration, Integral a) => a
blockRetrievalQueueSize =
    fromIntegral . ccBlockRetrievalQueueSize $ nodeConfiguration

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

----------------------------------------------------------------------------
-- Explorer parameters
----------------------------------------------------------------------------

-- | If 'True', explorer extended API, like fetching utxos for address is enabled.
-- WARNING Those endpoints are potentially expensive!
explorerExtendedApi :: HasNodeConfiguration => Bool
explorerExtendedApi = ccExplorerExtendedApi $ nodeConfiguration
