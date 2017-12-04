{-# LANGUAGE Rank2Types #-}

-- | Configuration part of block processing.

module Pos.Block.Configuration
    ( BlockConfiguration (..)
    , HasBlockConfiguration
    , blockConfiguration
    , withBlockConfiguration

    -- * Constants mentioned in paper
    , networkDiameter

    -- * Other constants
    , recoveryHeadersMessage
    ) where

import           Universum

import           Data.Aeson (FromJSON (..), genericParseJSON)
import           Data.Reflection (Given (..), give)
import           Data.Time.Units (Microsecond)
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util (sec)

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
    } deriving (Show, Generic)

instance FromJSON BlockConfiguration where
    parseJSON = genericParseJSON defaultOptions

----------------------------------------------------------------------------
-- Main constants mentioned in paper
----------------------------------------------------------------------------

-- | Estimated time needed to broadcast message from one node to all
-- other nodes. Also see 'Pos.NodeConfiguration.ccNetworkDiameter'.
networkDiameter :: HasBlockConfiguration => Microsecond
networkDiameter = sec . ccNetworkDiameter $ blockConfiguration

----------------------------------------------------------------------------
-- Other constants
----------------------------------------------------------------------------

-- | Maximum amount of headers node can put into headers message while
-- in "after offline" or "recovery" mode. Should be more than
-- 'blkSecurityParam'.
recoveryHeadersMessage :: (HasBlockConfiguration, Integral a) => a
recoveryHeadersMessage = fromIntegral . ccRecoveryHeadersMessage $ blockConfiguration
