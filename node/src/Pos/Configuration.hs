{-# LANGUAGE Rank2Types #-}

-- | Configuration used by the algorithm. See the paper for more details.

module Pos.Configuration
       ( NodeConfiguration (..)
       , HasNodeConfiguration
       , InputSelectionPolicyConf (..)
       , nodeConfiguration
       , withNodeConfiguration

       -- * Constants mentioned in paper
       , networkDiameter

       -- * Other constants
       , networkConnectionTimeout
       , blockRetrievalQueueSize
       , propagationQueueSize
       , defaultPeers
       , recoveryHeadersMessage
       , messageCacheTimeout

       -- * Delegation
       , lightDlgConfirmationTimeout
       , dlgCacheParam

       -- * Malicious activity detection constants
       , mdNoBlocksSlotThreshold

       -- * Transaction resubmition constants
       , pendingTxResubmitionPeriod

       -- * Transaction formation
       , txDefInputSelectionPolicy
       ) where

import           Universum

import           Data.Aeson             (FromJSON (..), genericParseJSON, withText)
import           Data.Reflection        (Given (..), give)
import           Data.Time.Units        (Microsecond, Second)
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util          (ms, sec)
import qualified Text.Parsec            as P

import           Pos.Util.TimeWarp      (NetworkAddress, addrParser)

type HasNodeConfiguration = Given NodeConfiguration

nodeConfiguration :: HasNodeConfiguration => NodeConfiguration
nodeConfiguration = given

withNodeConfiguration :: NodeConfiguration -> (HasNodeConfiguration => r) -> r
withNodeConfiguration = give

-- | Compile time configuration. See example in /constants.yaml/ file.
data NodeConfiguration = NodeConfiguration
    {
      ccNetworkDiameter             :: !Int
      -- ^ Estimated time for broadcasting messages
    , ccDefaultPeers                :: ![Text]
      -- ^ List of default peers
    , ccMdNoBlocksSlotThreshold     :: !Int
      -- ^ Threshold of slots for malicious activity detection
    , ccLightDlgConfirmationTimeout :: !Int
      -- ^ Timeout for holding light psks confirmations
    , ccDlgCacheParam               :: !Int
      -- ^ This value parameterizes size of cache used in Delegation.
      -- Not bytes, but number of elements.
    , ccRecoveryHeadersMessage      :: !Int
      -- ^ Numbers of headers put in message in recovery mode.
    , ccMessageCacheTimeout         :: !Int
      -- ^ Interval we ignore cached messages in components that
      -- support caching
    , ccNetworkConnectionTimeout    :: !Int
      -- ^ Network connection timeout in milliseconds
    , ccBlockRetrievalQueueSize     :: !Int
      -- ^ Block retrieval queue capacity
    , ccPropagationQueueSize        :: !Int
      -- ^ InvMsg propagation queue capacity
    , ccPendingTxResubmissionPeriod :: !Int
      -- ^ Minimal delay between pending transactions resubmission
    , ccTxDefInputSelectionPolicy   :: !InputSelectionPolicyConf
      -- ^ Default policy to pick inputs when forming transaction
    } deriving (Show, Generic)

instance FromJSON NodeConfiguration where
    parseJSON = genericParseJSON defaultOptions

-- | Specifies the way Uxtos are going to be grouped.
-- This maps 1-to-1 to @InputSelectionPolicy@, but has another 'FromJSON'
-- instance which parses from String.
data InputSelectionPolicyConf
    = OptimizeForSecurityConf  -- ^ Spend everything from the address
    | OptimizeForSizeConf      -- ^ No grouping
    deriving (Show, Eq, Generic)

instance FromJSON InputSelectionPolicyConf where
    parseJSON = withText "InputSelectionPolicy" $ \case
        "OptimizeForSecurity" -> pure OptimizeForSecurityConf
        "OptimizeForSize"     -> pure OptimizeForSizeConf
        _ -> fail "Unknown transaction inputs grouping policy"

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

-- | Timeout for caching system. Components that use caching on
-- messages can use this timeout to invalidate caches.
messageCacheTimeout :: (HasNodeConfiguration, Integral a) => a
messageCacheTimeout = fromIntegral . ccMessageCacheTimeout $ nodeConfiguration

----------------------------------------------------------------------------
-- Delegation
----------------------------------------------------------------------------

-- | Amount of time we hold confirmations for light PSKs.
lightDlgConfirmationTimeout :: (HasNodeConfiguration, Integral a) => a
lightDlgConfirmationTimeout = fromIntegral . ccLightDlgConfirmationTimeout $ nodeConfiguration

-- | This value parameterizes size of cache used in Delegation.
-- Not bytes, but number of elements.
dlgCacheParam :: (HasNodeConfiguration, Integral n) => n
dlgCacheParam = fromIntegral . ccDlgCacheParam $ nodeConfiguration

----------------------------------------------------------------------------
-- Malicious activity
----------------------------------------------------------------------------

-- | Number of slots used by malicious actions detection to check if
-- we are not receiving generated blocks.
mdNoBlocksSlotThreshold :: (HasNodeConfiguration, Integral i) => i
mdNoBlocksSlotThreshold = fromIntegral . ccMdNoBlocksSlotThreshold $ nodeConfiguration

----------------------------------------------------------------------------
-- Transactions formation
----------------------------------------------------------------------------

-- | Specifies how inputs should be selected when forming transaction.
-- This is default behaviour and may be overriden.
txDefInputSelectionPolicy :: HasNodeConfiguration => InputSelectionPolicyConf
txDefInputSelectionPolicy = ccTxDefInputSelectionPolicy $ nodeConfiguration

----------------------------------------------------------------------------
-- Transactions resubmition
----------------------------------------------------------------------------

pendingTxResubmitionPeriod :: HasNodeConfiguration => Second
pendingTxResubmitionPeriod = fromIntegral . ccPendingTxResubmissionPeriod $ nodeConfiguration
