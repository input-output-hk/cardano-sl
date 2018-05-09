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
import           Pos.Binary.Core ()
import           Pos.Core (EpochIndex, HasProtocolConstants, SlotId (SlotId),
                           SlotLeaders, StakeholderId, flattenSlotId, unsafeMkLocalSlotIndex,
                           HasGeneratedSecrets, HasGenesisData)
import           Pos.DB.Class (MonadDB, MonadDBRead)
import           Pos.Lrc.DB.Common (dbHasKey, getBi, putBatch, putBatchBi, putBi, toRocksOps)
import           Pos.Lrc.Genesis (genesisLeaders)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getLeadersForEpoch :: MonadDBRead m => EpochIndex -> m (Maybe SlotLeaders)
getLeadersForEpoch = getBi . leadersForEpochKey

getLeader :: (MonadDBRead m, HasProtocolConstants) => SlotId -> m (Maybe StakeholderId)
getLeader = getBi . leaderKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Put leaders for all slots in the specified epoch in the DB.
-- The DB contains two mappings:
-- * EpochIndex -> SlotLeaders
-- * SlotId -> StakeholderId (added in CSE-240)
putLeadersForEpoch :: (MonadDB m, HasProtocolConstants) => EpochIndex -> SlotLeaders -> m ()
putLeadersForEpoch epoch leaders = do
    let opsAllAtOnce  = toRocksOps $ putLeadersForEpochAllAtOnceOps epoch leaders
        opsSeparately = toRocksOps $ putLeadersForEpochSeparatelyOps epoch leaders
    putBatch $ opsAllAtOnce <> opsSeparately

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcLeaders ::
       ( MonadDB m
       , HasProtocolConstants
       , HasGeneratedSecrets
       , HasGenesisData
       )
    => m ()
prepareLrcLeaders =
    -- Initialization flag was added with CSE-240.
    unlessM isLrcDbInitialized $ do
        hasLeadersForEpoch0 <- hasLeaders 0
        if not hasLeadersForEpoch0 then
            -- The node is not initialized at all. Only need to put leaders
            -- for the first epoch.
            putLeadersForEpoch 0 genesisLeaders
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
                putBatchBi $ putLeadersForEpochSeparatelyOps i leaders
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

leaderKey :: HasProtocolConstants => SlotId -> ByteString
leaderKey = mappend "ls/" . serialize' . flattenSlotId

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
    :: HasProtocolConstants
    => EpochIndex
    -> SlotLeaders
    -> [(ByteString, StakeholderId)]
putLeadersForEpochSeparatelyOps epoch leaders =
    [(leaderKey $ mkSlotId epoch i, leader)
    | (i, leader) <- zip [0..] $ toList leaders]
  where
    mkSlotId :: HasProtocolConstants => EpochIndex -> Word16 -> SlotId
    mkSlotId epoch' slot =
        -- Using @unsafeMkLocalSlotIndex@ because we trust the callers.
        SlotId epoch' (unsafeMkLocalSlotIndex slot)
