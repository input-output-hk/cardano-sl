-- | Protocol constants and some derived terms.

module Pos.Core.ProtocolConstants
       ( ProtocolConstants (..)
       , VssMinTTL (..)
       , VssMaxTTL (..)

       , vssMaxTTL
       , vssMinTTL

       , pcBlkSecurityParam

       , pcSlotSecurityParam
       , kSlotSecurityParam

       , pcChainQualityThreshold
       , kChainQualityThreshold

       , pcEpochSlots
       , kEpochSlots
       ) where

import           Universum

import           Pos.Core.Common (BlockCount (..))
import           Pos.Core.Slotting.SlotCount (SlotCount)

-- | The 'k' parameter and TTLs for VSS certificates.
data ProtocolConstants = ProtocolConstants
    { -- | Security parameter from the paper.
      pcK         :: !Int
      -- | VSS certificates min timeout to live (number of epochs).
    , pcVssMinTTL :: !VssMinTTL
      -- | VSS certificates max timeout to live (number of epochs).
    , pcVssMaxTTL :: !VssMaxTTL
    } deriving (Show, Eq, Generic)

-- | Minimum time-to-live for a VSS certificate.
newtype VssMinTTL = VssMinTTL
    { getVssMinTTL :: Word32
    } deriving (Eq, Show, Bounded, Enum, Generic)

-- | Maximum time-to-live for a VSS certificate.
newtype VssMaxTTL = VssMaxTTL
    { getVssMaxTTL :: Word32
    } deriving (Eq, Show, Bounded, Enum, Generic)

-- | VSS certificates max timeout to live (number of epochs)
vssMaxTTL :: Integral i => ProtocolConstants -> i
vssMaxTTL = fromIntegral . getVssMaxTTL . pcVssMaxTTL

-- | VSS certificates min timeout to live (number of epochs)
vssMinTTL :: Integral i => ProtocolConstants -> i
vssMinTTL = fromIntegral . getVssMinTTL . pcVssMinTTL

-- | Security parameter which is maximum number of blocks which can be
-- rolled back.
pcBlkSecurityParam :: ProtocolConstants -> BlockCount
pcBlkSecurityParam = fromIntegral . pcK

-- | Security parameter expressed in number of slots. It uses chain
-- quality property. It's basically @blkSecurityParam / chainQualityThreshold@.
pcSlotSecurityParam :: ProtocolConstants -> SlotCount
pcSlotSecurityParam = kSlotSecurityParam . pcBlkSecurityParam

kSlotSecurityParam :: BlockCount -> SlotCount
kSlotSecurityParam = fromIntegral . (*) 2 . getBlockCount

-- We don't have a special newtype for it, so it can be any
-- 'Fractional'. I think adding newtype here would be overkill
-- (@gromak). Also this value is not actually part of the protocol,
-- but rather implementation detail, so we don't need to ensure
-- conrete precision. Apart from that, in reality we know that it's
-- 0.5, so any fractional type should be fine ☺
--
-- | Minimal chain quality (number of blocks divided by number of
-- slots) necessary for security of the system.
pcChainQualityThreshold :: Fractional f => ProtocolConstants -> f
pcChainQualityThreshold = kChainQualityThreshold . pcBlkSecurityParam

kChainQualityThreshold :: Fractional f => BlockCount -> f
kChainQualityThreshold k = realToFrac k / realToFrac (kSlotSecurityParam k)

-- | Number of slots inside one epoch.
--
-- FIXME strange that it's defined in terms of block security param.
-- Shouldn't it be the other way around?
pcEpochSlots :: ProtocolConstants -> SlotCount
pcEpochSlots = kEpochSlots . pcBlkSecurityParam

kEpochSlots :: BlockCount -> SlotCount
kEpochSlots = fromIntegral . (*) 10 . getBlockCount
