{-# LANGUAGE Rank2Types #-}

-- | Configuration part of block processing.

module Pos.Chain.Block.Configuration
    ( BlockConfiguration (..)
    , HasBlockConfiguration
    , blockConfiguration
    , withBlockConfiguration

    -- * Constants mentioned in paper
    , networkDiameter

    -- * Chain quality
    , nonCriticalCQBootstrap
    , criticalCQBootstrap
    , nonCriticalCQ
    , criticalCQ
    , criticalForkThreshold
    , fixedTimeCQ
    , fixedTimeCQSec

    -- * Other constants
    , recoveryHeadersMessage
    , streamWindow
    ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON,
                     genericToJSON)
import           Data.Aeson.Options (defaultOptions)
import           Data.Reflection (Given (..), give)
import           Data.Time.Units (Microsecond, Second, convertUnit,
                     fromMicroseconds)

import           Pos.Core.Aeson ()

type HasBlockConfiguration = Given BlockConfiguration

blockConfiguration :: HasBlockConfiguration => BlockConfiguration
blockConfiguration = given

withBlockConfiguration :: BlockConfiguration -> (HasBlockConfiguration => r) -> r
withBlockConfiguration = give

data BlockConfiguration = BlockConfiguration
    {
      ccNetworkDiameter        :: !Int
      -- ^ Estimated time for broadcasting messages
    , ccRecoveryHeadersMessage :: !Int
      -- ^ Numbers of headers put in message in recovery mode.
    , ccStreamWindow           :: !Int
      -- ^ Number of blocks to have inflight

      -- Chain quality thresholds and other constants to detect
      -- suspicious things.

      -- | If chain quality in bootstrap era is less than this value,
      -- non critical misbehavior will be reported.
    , ccNonCriticalCQBootstrap :: !Double
      -- | If chain quality in bootstrap era is less than this value,
      -- critical misbehavior will be reported.
    , ccCriticalCQBootstrap    :: !Double
      -- | If chain quality after bootstrap era is less than this
      -- value, non critical misbehavior will be reported.
    , ccNonCriticalCQ          :: !Double
      -- | If chain quality after bootstrap era is less than this
      -- value, critical misbehavior will be reported.
    , ccCriticalCQ             :: !Double
      -- | Number of blocks such that if so many blocks are rolled
      -- back, it requires immediate reaction.
    , ccCriticalForkThreshold  :: !Int
      -- | Chain quality will be also calculated for this amount of seconds.
    , ccFixedTimeCQ            :: !Second

    } deriving (Show, Generic)

instance ToJSON BlockConfiguration where
    toJSON = genericToJSON defaultOptions

instance FromJSON BlockConfiguration where
    parseJSON = genericParseJSON defaultOptions

----------------------------------------------------------------------------
-- Main constants mentioned in paper
----------------------------------------------------------------------------

-- | Estimated time needed to broadcast message from one node to all
-- other nodes. Also see 'Pos.NodeConfiguration.ccNetworkDiameter'.
networkDiameter :: HasBlockConfiguration => Microsecond
networkDiameter = fromMicroseconds . (*) 1000000 . fromIntegral . ccNetworkDiameter $ blockConfiguration

----------------------------------------------------------------------------
-- Chain quality
----------------------------------------------------------------------------

-- | If chain quality in bootstrap era is less than this value,
-- non critical misbehavior will be reported.
nonCriticalCQBootstrap :: HasBlockConfiguration => Double
nonCriticalCQBootstrap = ccNonCriticalCQBootstrap blockConfiguration

-- | If chain quality in bootstrap era is less than this value,
-- critical misbehavior will be reported.
criticalCQBootstrap :: HasBlockConfiguration => Double
criticalCQBootstrap = ccCriticalCQBootstrap blockConfiguration

-- | If chain quality after bootstrap era is less than this
-- value, non critical misbehavior will be reported.
nonCriticalCQ :: HasBlockConfiguration => Double
nonCriticalCQ = ccNonCriticalCQ blockConfiguration

-- | If chain quality after bootstrap era is less than this
-- value, critical misbehavior will be reported.
criticalCQ :: HasBlockConfiguration => Double
criticalCQ = ccCriticalCQ blockConfiguration

-- | If chain quality after bootstrap era is less than this
-- value, critical misbehavior will be reported.
criticalForkThreshold :: (HasBlockConfiguration, Integral i) => i
criticalForkThreshold = fromIntegral . ccCriticalForkThreshold $ blockConfiguration

-- | Chain quality will be also calculated for this amount of time.
fixedTimeCQ :: HasBlockConfiguration => Microsecond
fixedTimeCQ = convertUnit fixedTimeCQSec

-- | 'fixedTimeCQ' expressed as seconds.
fixedTimeCQSec :: HasBlockConfiguration => Second
fixedTimeCQSec = ccFixedTimeCQ blockConfiguration

----------------------------------------------------------------------------
-- Other constants
----------------------------------------------------------------------------

-- | Maximum amount of headers node can put into headers message while
-- in "after offline" or "recovery" mode. Should be more than
-- 'blkSecurityParam'.
recoveryHeadersMessage :: (HasBlockConfiguration, Integral a) => a
recoveryHeadersMessage = fromIntegral . ccRecoveryHeadersMessage $ blockConfiguration

-- | The maximum number of blocks to have in flight.
-- Provides back-preassure from client to server when streaming.
streamWindow :: (HasBlockConfiguration, Integral a) => a
streamWindow = fromIntegral . ccStreamWindow $ blockConfiguration

