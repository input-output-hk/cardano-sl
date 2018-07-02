-- | Leaders part of LRC DB.

module Pos.Lrc.DB.Leaders
       (
         -- * Getters
         getLeadersForEpoch
       , getLeader

       -- * Operations
       , putLeadersForEpoch

       -- * Initialization
       , prepareLrcLeaders

       -- * Helpers
       , hasLeaders
       ) where

import           Universum

import           Pos.Binary.Class (serialize')
import           Pos.Core (EpochIndex, SlotCount, SlotId (SlotId), SlotLeaders,
                     StakeholderId, flattenSlotId, unsafeMkLocalSlotIndex)
import           Pos.DB.Class (MonadDB, MonadDBRead)
import           Pos.Lrc.DB.Common (dbHasKey, getBi, putBatch, putBatchBi,
                     putBi, toRocksOps)
import           Pos.Lrc.Genesis (genesisLeaders)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getLeadersForEpoch :: MonadDBRead m => EpochIndex -> m (Maybe SlotLeaders)
getLeadersForEpoch = getBi . leadersForEpochKey

getLeader :: MonadDBRead m => SlotCount -> SlotId -> m (Maybe StakeholderId)
getLeader epochSlots = getBi . leaderKey epochSlots

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Put leaders for all slots in the specified epoch in the DB.
-- The DB contains two mappings:
-- * EpochIndex -> SlotLeaders
-- * SlotId -> StakeholderId (added in CSE-240)
putLeadersForEpoch :: MonadDB m => SlotCount -> EpochIndex -> SlotLeaders -> m ()
putLeadersForEpoch epochSlots epoch leaders = do
    let opsAllAtOnce  = toRocksOps $ putLeadersForEpochAllAtOnceOps epoch leaders
        opsSeparately = toRocksOps $ putLeadersForEpochSeparatelyOps epochSlots epoch leaders
    putBatch $ opsAllAtOnce <> opsSeparately

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcLeaders :: MonadDB m => SlotCount -> m ()
prepareLrcLeaders epochSlots =
    -- Initialization flag was added with CSE-240.
    unlessM isLrcDbInitialized $ do
        hasLeadersForEpoch0 <- hasLeaders 0
        if not hasLeadersForEpoch0 then
            -- The node is not initialized at all. Only need to put leaders
            -- for the first epoch.
            putLeadersForEpoch epochSlots 0 (genesisLeaders epochSlots)
        else
            -- The node was initialized before CSE-240.
            -- Need to migrate data for all epochs.
            initLeaders 0
        putInitFlag
  where
    initLeaders :: MonadDB m => EpochIndex -> m ()
    initLeaders i = do
        maybeLeaders <- getLeadersForEpoch i
        case maybeLeaders of
            Just leaders -> do
                putBatchBi $ putLeadersForEpochSeparatelyOps epochSlots i leaders
                initLeaders (i + 1)
            Nothing -> pure ()

isLrcDbInitialized :: MonadDB m => m Bool
isLrcDbInitialized = dbHasKey lrcDbLeadersInitFlag

putInitFlag :: MonadDB m => m ()
putInitFlag = putBi lrcDbLeadersInitFlag ()

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersForEpochKey :: EpochIndex -> ByteString
leadersForEpochKey = mappend "l/" . serialize'

leaderKey :: SlotCount -> SlotId -> ByteString
leaderKey epochSlots = mappend "ls/" . serialize' . flattenSlotId epochSlots

lrcDbLeadersInitFlag :: ByteString
lrcDbLeadersInitFlag = "linit/"

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

hasLeaders :: MonadDBRead m => EpochIndex -> m Bool
hasLeaders = dbHasKey . leadersForEpochKey

putLeadersForEpochAllAtOnceOps
    :: EpochIndex
    -> SlotLeaders
    -> [(ByteString, SlotLeaders)]
putLeadersForEpochAllAtOnceOps epoch leaders =
    [(leadersForEpochKey epoch, leaders)]

putLeadersForEpochSeparatelyOps
    :: SlotCount
    -> EpochIndex
    -> SlotLeaders
    -> [(ByteString, StakeholderId)]
putLeadersForEpochSeparatelyOps epochSlots epoch leaders =
    [(leaderKey epochSlots $ mkSlotId epoch i, leader)
    | (i, leader) <- zip [0..] $ toList leaders]
  where
    mkSlotId :: EpochIndex -> Word16 -> SlotId
    mkSlotId epoch' slot =
        -- Using @unsafeMkLocalSlotIndex@ because we trust the callers.
        SlotId epoch' (unsafeMkLocalSlotIndex epochSlots slot)
