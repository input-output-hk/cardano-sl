{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.Protocol
       (
         HasProtocolConstants
       , withProtocolConstants
       , protocolConstants
       , vssMaxTTL
       , vssMinTTL
       , blkSecurityParam
       , slotSecurityParam
       , epochSlots
       , chainQualityThreshold

       ) where

import           Universum

import           Data.Reflection (Given (..), give)

import           Pos.Core.Common (BlockCount (..))
import           Pos.Core.ProtocolConstants (ProtocolConstants (..), VssMaxTTL (..),
                                             VssMinTTL (..))
import           Pos.Core.Slotting.Types (SlotCount)

type HasProtocolConstants = Given ProtocolConstants

withProtocolConstants ::
       ProtocolConstants
    -> (HasProtocolConstants => r)
    -> r
withProtocolConstants = give

protocolConstants :: HasProtocolConstants => ProtocolConstants
protocolConstants = given

-- | VSS certificates max timeout to live (number of epochs)
vssMaxTTL :: (HasProtocolConstants, Integral i) => i
vssMaxTTL = fromIntegral . getVssMaxTTL . pcVssMaxTTL $ protocolConstants

-- | VSS certificates min timeout to live (number of epochs)
vssMinTTL :: (HasProtocolConstants, Integral i) => i
vssMinTTL = fromIntegral . getVssMinTTL . pcVssMinTTL $ protocolConstants

-- | Security parameter which is maximum number of blocks which can be
-- rolled back.
blkSecurityParam :: HasProtocolConstants => BlockCount
blkSecurityParam = fromIntegral . pcK $ protocolConstants

-- | Security parameter expressed in number of slots. It uses chain
-- quality property. It's basically @blkSecurityParam / chainQualityThreshold@.
slotSecurityParam :: HasProtocolConstants => SlotCount
slotSecurityParam = fromIntegral $ 2 * getBlockCount blkSecurityParam

-- We don't have a special newtype for it, so it can be any
-- 'Fractional'. I think adding newtype here would be overkill
-- (@gromak). Also this value is not actually part of the protocol,
-- but rather implementation detail, so we don't need to ensure
-- conrete precision. Apart from that, in reality we know that it's
-- 0.5, so any fractional type should be fine ☺
--
-- | Minimal chain quality (number of blocks divided by number of
-- slots) necessary for security of the system.
chainQualityThreshold :: (HasProtocolConstants, Fractional fractional) => fractional
chainQualityThreshold =
    realToFrac blkSecurityParam / realToFrac slotSecurityParam

-- | Number of slots inside one epoch.
epochSlots :: (HasProtocolConstants) => SlotCount
epochSlots = fromIntegral $ 10 * getBlockCount blkSecurityParam
