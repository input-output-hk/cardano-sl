module Pos.Chain.Lrc.OBFT
        ( getSlotLeaderObftPure
        , getEpochSlotLeaderScheduleObftPure
        ) where

import           Universum hiding (sort)

import           Data.List.NonEmpty ((!!))
import qualified Data.List.NonEmpty as NE (iterate, sort, take)

import           Pos.Core (EpochIndex, FlatSlotId, LocalSlotIndex (..),
                     SlotCount (..), SlotId (..), SlotLeaders, StakeholderId,
                     flattenEpochOrSlot, slotIdSucc)

-- | Selects the StakeholderId that matches the @SlotId@ index in a
-- @SlotCount@-length epoch.
getSlotLeaderObftPure
    :: SlotId
    -> SlotCount
    -> NonEmpty StakeholderId
    -> StakeholderId
getSlotLeaderObftPure slotId slotCount stakeholders =
    sortedStakeholders !! leaderIndex
  where
    -- Ensure the stakeholders are sorted
    sortedStakeholders :: NonEmpty StakeholderId
    sortedStakeholders = NE.sort stakeholders
    --
    leaderIndex :: Int
    leaderIndex = (fromIntegral flatSlotId :: Int) `mod` (length stakeholders)
    --
    flatSlotId :: FlatSlotId
    flatSlotId = flattenEpochOrSlot slotCount slotId

-- | Selects @SlotCount@ StakeholderIds for the given epoch @EpochIndex@.
getEpochSlotLeaderScheduleObftPure
    :: EpochIndex
    -> SlotCount
    -> NonEmpty StakeholderId
    -> SlotLeaders
getEpochSlotLeaderScheduleObftPure epochIndex epochSlotCount stakeholders =
    case nonEmpty slotLeaderSchedule of
        Just sls -> sls
        Nothing  -> error "getEpochSlotLeaderScheduleObftPure: Empty slot leader schedule"
  where
    slotLeaderSchedule =
        map (\si -> getSlotLeaderObftPure si epochSlotCount stakeholders)
            (NE.take (fromIntegral $ numEpochSlots)
                     (NE.iterate (slotIdSucc epochSlotCount) startSlotId))
    --
    startSlotId :: SlotId
    startSlotId = SlotId epochIndex (UnsafeLocalSlotIndex 0)
    --
    numEpochSlots :: Word64
    numEpochSlots = getSlotCount $ epochSlotCount
