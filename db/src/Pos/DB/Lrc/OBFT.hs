module Pos.DB.Lrc.OBFT
       ( getSlotLeaderObft
       ) where

import           Universum

import qualified Data.Map as M (toList)
import           Pos.Chain.Delegation (ProxySKBlockInfo)
import           Pos.Chain.Genesis (GenesisData (..), GenesisWStakeholders (..))
import qualified Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Core (FlatSlotId, SlotId, StakeholderId, addressHash,
                     flattenEpochOrSlot, pcEpochSlots)
import           Pos.Crypto (ProxySecretKey (..))
import           Pos.DB (MonadDBRead)
import           Pos.DB.Delegation (getDlgTransPsk)

import           Data.List ((!!))
import           UnliftIO (MonadUnliftIO)

-- | This function selects the current slot leaders by obtaining the
-- genesis stakeholders, then tracing them through the delegation
-- mapping. If a genesis stakeholder delegated to another stakeholder,
-- we return the delegatee's id. If the genesis stakeholder did not
-- delegate, we return their id.
getSlotLeaderObft
    :: (MonadDBRead m, MonadUnliftIO m)
    => Genesis.Config -> SlotId -> m (StakeholderId, ProxySKBlockInfo)
getSlotLeaderObft genesisConfig si = do
    mDlg <- getDlgTransPsk currentSlotGenesisSId
    case mDlg of
        Just (_, UnsafeProxySecretKey { pskDelegatePk = delegateePk }) ->
            pure ((addressHash delegateePk), (swap <$> mDlg))
        Nothing -> pure (currentSlotGenesisSId, Nothing)
  where
    -- We assume here that the genesis bootstrap stakeholders list
    -- is nonempty
    stakeholders :: [StakeholderId]
    stakeholders = sort $
                    map fst $
                    M.toList $
                    getGenesisWStakeholders $
                    gdBootStakeholders $
                    Genesis.configGenesisData genesisConfig
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
