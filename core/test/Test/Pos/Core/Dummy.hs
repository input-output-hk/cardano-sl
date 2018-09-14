module Test.Pos.Core.Dummy
       ( dummyProtocolConstants
       , dummyK
       , dummyEpochSlots
       , dummySlotSecurityParam
       ) where

import           Pos.Core (BlockCount, ProtocolConstants (..), SlotCount,
                     VssMaxTTL (..), VssMinTTL (..), kEpochSlots,
                     kSlotSecurityParam, pcBlkSecurityParam)

dummyProtocolConstants :: ProtocolConstants
dummyProtocolConstants = ProtocolConstants
    { pcK         = 10
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

dummyK :: BlockCount
dummyK = pcBlkSecurityParam dummyProtocolConstants

dummyEpochSlots :: SlotCount
dummyEpochSlots = kEpochSlots dummyK

dummySlotSecurityParam :: SlotCount
dummySlotSecurityParam = kSlotSecurityParam dummyK
