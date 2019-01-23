module Pos.DB.Lrc.OBFT
       ( getSlotLeaderObft
       , getEpochSlotLeaderScheduleObft
       ) where

import           Universum

import           Pos.Chain.Delegation (ProxySKBlockInfo)
import           Pos.Chain.Genesis (configGenesisWStakeholders)
import qualified Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Lrc (getEpochSlotLeaderScheduleObftPure,
                     getSlotLeaderObftPure)
import           Pos.Core (EpochIndex, SlotCount (..), SlotId (..), SlotLeaders,
                     StakeholderId, pcEpochSlots)
import           Pos.DB (MonadDBRead)
import           Pos.DB.Delegation (getDlgTransPsk)

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
    epochSlotCount :: SlotCount
    epochSlotCount =
        pcEpochSlots (Genesis.configProtocolConstants genesisConfig)
    --
    currentSlotGenesisSId :: StakeholderId
    currentSlotGenesisSId =
        case (nonEmpty stakeholders) of
            Just s  -> getSlotLeaderObftPure si epochSlotCount s
            Nothing -> error "getSlotLeaderObft: Empty list of stakeholders"

-- | Generates the full slot leader schedule for an epoch (10*k slots long).
getEpochSlotLeaderScheduleObft
    :: Genesis.Config -> EpochIndex -> SlotLeaders
getEpochSlotLeaderScheduleObft genesisConfig ei =
    case nonEmpty stakeholders of
        Just s  -> getEpochSlotLeaderScheduleObftPure ei epochSlotCount s
        Nothing -> error "getEpochSlotLeaderScheduleObft: Empty list of stakeholders"
  where
    -- We assume here that the genesis bootstrap stakeholders list
    -- is nonempty
    stakeholders :: [StakeholderId]
    stakeholders = sort $ configGenesisWStakeholders genesisConfig
    epochSlotCount = pcEpochSlots (Genesis.configProtocolConstants genesisConfig)
