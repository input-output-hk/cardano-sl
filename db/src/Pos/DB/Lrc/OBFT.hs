module Pos.DB.Lrc.OBFT
       ( getSlotLeaderObft
       , getEpochSlotLeaderScheduleObft
       ) where

import           Universum

import           Pos.Chain.Delegation (ProxySKBlockInfo)
import           Pos.Chain.Genesis (configGenesisWStakeholders)
import qualified Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Core (EpochIndex, FlatSlotId, LocalSlotIndex (..),
                     SlotCount (..), SlotId (..), SlotLeaders, StakeholderId,
                     flattenEpochOrSlot, pcEpochSlots, slotIdSucc)
import           Pos.DB (MonadDBRead)
import           Pos.DB.Delegation (getDlgTransPsk)

import           Data.List ((!!))
import           UnliftIO (MonadUnliftIO)

-- | This function selects the current slot leaders by obtaining the
-- genesis stakeholders, then tracing them through the delegation
-- mapping.
getSlotLeaderObft
    :: (MonadDBRead m, MonadUnliftIO m)
    => Genesis.Config -> SlotId -> m (StakeholderId, ProxySKBlockInfo)
getSlotLeaderObft genesisConfig si = do
    mDlg <- getDlgTransPsk currentSlotGenesisSId
    pure (currentSlotGenesisSId, (swap <$> mDlg))
  where
    -- We assume here that the genesis bootstrap stakeholders list
    -- is nonempty
    stakeholders :: [StakeholderId]
    stakeholders = sort $ configGenesisWStakeholders genesisConfig
    --
    flatSlotId :: FlatSlotId
    flatSlotId =
        flattenEpochOrSlot (pcEpochSlots (Genesis.configProtocolConstants
                                              genesisConfig))
                           si
    --
    leaderIndex :: Int
    leaderIndex = (fromIntegral flatSlotId :: Int) `mod` (length stakeholders)
    --
    currentSlotGenesisSId :: StakeholderId
    currentSlotGenesisSId = stakeholders !! leaderIndex

-- | Generates the full slot leader schedule for an epoch (10*k slots long).
getEpochSlotLeaderScheduleObft
    :: (MonadDBRead m, MonadUnliftIO m)
    => Genesis.Config -> EpochIndex -> m SlotLeaders
getEpochSlotLeaderScheduleObft genesisConfig ei = do
    leaders <-
        map fst
            <$> mapM (getSlotLeaderObft genesisConfig)
                     (take (fromIntegral $ epochSlotCount)
                           (iterate (slotIdSucc epochSlots) startSlotId))
    case nonEmpty leaders of
        Just l  -> pure l
        Nothing -> error "getEpochSlotLeaderScheduleObft: Empty list of leaders"
  where
    startSlotId = SlotId ei (UnsafeLocalSlotIndex 0)
    epochSlots = pcEpochSlots (Genesis.configProtocolConstants genesisConfig)
    epochSlotCount = getSlotCount $ epochSlots
