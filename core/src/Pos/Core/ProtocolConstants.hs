-- | Protocol constants and some derived terms.

module Pos.Core.ProtocolConstants
    ( ProtocolConstants (..)
    , VssMinTTL (..)
    , VssMaxTTL (..)

    , pcBlkSecurityParam
    , pcSlotSecurityParam
    , pcChainQualityThreshold
    , pcEpochSlots
    ) where

import           Universum

import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Core.Common (BlockCount (..))
import           Pos.Core.Genesis.Canonical ()
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

instance Monad m => ToJSON m VssMinTTL where
    toJSON = toJSON . getVssMinTTL

instance (ReportSchemaErrors m) => FromJSON m VssMinTTL where
    fromJSON = fmap VssMinTTL . fromJSON

instance Aeson.ToJSON VssMinTTL where
    toJSON = Aeson.toJSON . getVssMinTTL

instance Aeson.FromJSON VssMinTTL where
    parseJSON = fmap VssMinTTL . Aeson.parseJSON

-- | Maximum time-to-live for a VSS certificate.
newtype VssMaxTTL = VssMaxTTL
    { getVssMaxTTL :: Word32
    } deriving (Eq, Show, Bounded, Enum, Generic)

instance Monad m => ToJSON m VssMaxTTL where
    toJSON = toJSON . getVssMaxTTL

instance (ReportSchemaErrors m) => FromJSON m VssMaxTTL where
    fromJSON = fmap VssMaxTTL . fromJSON

instance Aeson.ToJSON VssMaxTTL where
    toJSON = Aeson.toJSON . getVssMaxTTL

instance Aeson.FromJSON VssMaxTTL where
    parseJSON = fmap VssMaxTTL . Aeson.parseJSON

-- | Security parameter which is maximum number of blocks which can be
-- rolled back.
pcBlkSecurityParam :: ProtocolConstants -> BlockCount
pcBlkSecurityParam = fromIntegral . pcK

-- | Security parameter expressed in number of slots. It uses chain
-- quality property. It's basically @blkSecurityParam / chainQualityThreshold@.
pcSlotSecurityParam :: ProtocolConstants -> SlotCount
pcSlotSecurityParam = fromIntegral . (*) 2 . getBlockCount . pcBlkSecurityParam

-- We don't have a special newtype for it, so it can be any
-- 'Fractional'. I think adding newtype here would be overkill
-- (@gromak). Also this value is not actually part of the protocol,
-- but rather implementation detail, so we don't need to ensure
-- conrete precision. Apart from that, in reality we know that it's
-- 0.5, so any fractional type should be fine ☺
--
-- | Minimal chain quality (number of blocks divided by number of
-- slots) necessary for security of the system.
pcChainQualityThreshold :: (Fractional fractional) => ProtocolConstants -> fractional
pcChainQualityThreshold pc =
    realToFrac (pcBlkSecurityParam pc) / realToFrac (pcSlotSecurityParam pc)

-- | Number of slots inside one epoch.
--
-- FIXME strange that it's defined in terms of block security param.
-- Shouldn't it be the other way around?
pcEpochSlots :: ProtocolConstants -> SlotCount
pcEpochSlots = fromIntegral . (*) 10 . getBlockCount . pcBlkSecurityParam
