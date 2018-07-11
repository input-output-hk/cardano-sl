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
import           Pos.Core (EpochIndex, ProtocolConstants, SlotCount,
                     SlotId (SlotId), SlotLeaders, StakeholderId,
                     CoreConfiguration, GenesisData,
                     flattenSlotId, pcEpochSlots,
                     unsafeMkLocalSlotIndexExplicit)
import           Pos.DB.Class (MonadDB, MonadDBRead)
import           Pos.Lrc.DB.Common (dbHasKey, getBi, putBatch, putBatchBi,
                     putBi, toRocksOps)
import           Pos.Lrc.Genesis (genesisLeaders)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getLeadersForEpoch :: MonadDBRead m => CoreConfiguration -> EpochIndex -> m (Maybe SlotLeaders)
getLeadersForEpoch cc = getBi cc . leadersForEpochKey

getLeader :: MonadDBRead m => CoreConfiguration -> ProtocolConstants -> SlotId -> m (Maybe StakeholderId)
getLeader cc pc = getBi cc . leaderKey pc

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Put leaders for all slots in the specified epoch in the DB.
-- The DB contains two mappings:
-- * EpochIndex -> SlotLeaders
-- * SlotId -> StakeholderId (added in CSE-240)
putLeadersForEpoch
  :: MonadDB m
  => CoreConfiguration -> ProtocolConstants -> EpochIndex -> SlotLeaders -> m ()
putLeadersForEpoch cc pc epoch leaders = do
    let opsAllAtOnce  = toRocksOps cc $ putLeadersForEpochAllAtOnceOps epoch leaders
        opsSeparately = toRocksOps cc $ putLeadersForEpochSeparatelyOps pc epoch leaders
    putBatch $ opsAllAtOnce <> opsSeparately

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcLeaders
  :: MonadDB m => CoreConfiguration -> GenesisData -> ProtocolConstants -> SlotCount -> m ()
prepareLrcLeaders cc gd pc epochSlots_ =
    -- Initialization flag was added with CSE-240.
    unlessM isLrcDbInitialized $ do
        hasLeadersForEpoch0 <- hasLeaders 0
        if not hasLeadersForEpoch0 then
            -- The node is not initialized at all. Only need to put leaders
            -- for the first epoch.
            putLeadersForEpoch cc pc 0 (genesisLeaders gd epochSlots_)
        else
            -- The node was initialized before CSE-240.
            -- Need to migrate data for all epochs.
            initLeaders 0
        putInitFlag cc
  where
    initLeaders :: MonadDB m => EpochIndex -> m ()
    initLeaders i = do
        maybeLeaders <- getLeadersForEpoch cc i
        case maybeLeaders of
            Just leaders -> do
                putBatchBi cc $ putLeadersForEpochSeparatelyOps pc i leaders
                initLeaders (i + 1)
            Nothing -> pure ()

isLrcDbInitialized :: MonadDB m => m Bool
isLrcDbInitialized = dbHasKey lrcDbLeadersInitFlag

putInitFlag :: MonadDB m => CoreConfiguration -> m ()
putInitFlag cc = putBi cc lrcDbLeadersInitFlag ()

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersForEpochKey :: EpochIndex -> ByteString
leadersForEpochKey = mappend "l/" . serialize'

leaderKey :: ProtocolConstants -> SlotId -> ByteString
leaderKey pc = mappend "ls/" . serialize' . flattenSlotId pc

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
    :: ProtocolConstants
    -> EpochIndex
    -> SlotLeaders
    -> [(ByteString, StakeholderId)]
putLeadersForEpochSeparatelyOps pc epoch leaders =
    [ (leaderKey pc $ mkSlotId epoch i, leader)
    | (i, leader) <- zip [0..] $ toList leaders]
  where
    mkSlotId :: EpochIndex -> Word16 -> SlotId
    mkSlotId epoch' slot =
        -- Using @unsafeMkLocalSlotIndexExplicit@ because we trust the callers.
        SlotId epoch' (unsafeMkLocalSlotIndexExplicit (pcEpochSlots pc) slot)
