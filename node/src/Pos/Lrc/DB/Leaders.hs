-- | Leaders part of LRC DB.

module Pos.Lrc.DB.Leaders
       (
         -- * Getters
         getLeadersForEpoch
       , getLeader

       -- * Operations
       , putLeadersForEpoch
       , putLeader

       -- * Initialization
       , prepareLrcLeaders
       ) where

import           Universum

import           Pos.Binary.Class      (serialize')
import           Pos.Binary.Core       ()
import           Pos.Context.Functions (genesisLeaders)
import           Pos.Core              (EpochIndex, HasConfiguration,
                                        HasProtocolConstants, SlotId (SlotId),
                                        SlotLeaders, StakeholderId, flattenSlotId,
                                        unsafeMkLocalSlotIndex)
import           Pos.DB.Class          (MonadDB, MonadDBRead)
import           Pos.Lrc.DB.Common     (dbHasKey, getBi, putBatch, putBatchBi, putBi,
                                        toRocksOps)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getLeadersForEpoch :: MonadDBRead m => EpochIndex -> m (Maybe SlotLeaders)
getLeadersForEpoch = getBi . leadersForEpochKey

getLeader :: MonadDBRead m => SlotId -> m (Maybe StakeholderId)
getLeader = getBi . leaderKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Put leaders for all slots in the specified epoch in the DB.
-- The DB contains two mappings:
-- * EpochIndex -> SlotLeaders
-- * SlotId -> StakeholderId (added in CSE-240)
putLeadersForEpoch :: MonadDB m => EpochIndex -> SlotLeaders -> m ()
putLeadersForEpoch epoch leaders = do
    let opsAllAtOnce  = toRocksOps $ putLeadersForEpochAllAtOnceOps epoch leaders
        opsSeparately = toRocksOps $ putLeadersForEpochSeparatelyOps epoch leaders
    putBatch $ opsAllAtOnce <> opsSeparately

putLeader :: MonadDB m => SlotId -> StakeholderId -> m ()
putLeader slot = putBi (leaderKey slot)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcLeaders ::
       ( MonadDB m
       , HasConfiguration
       )
    => m ()
prepareLrcLeaders = do
    -- Initialization flag was added with CSE-240.
    unlessM isLrcDbInitialized $ do
        hasLeadersForEpoch0 <- isJust <$> getLeadersForEpoch 0
        if not hasLeadersForEpoch0 then
            -- The node is not initialized at all. Only need to put leaders
            -- for the first epoch.
            putLeadersForEpoch 0 genesisLeaders
        else
            -- The node was initialized before CSE-240.
            -- Need to migrate data for all epochs.
            initLeaders 0
  where
    initLeaders :: MonadDB m => EpochIndex -> m ()
    initLeaders i = do
        whenNothingM_ (getLeader $ SlotId i minBound) $ do
            maybeLeaders <- getLeadersForEpoch i
            case maybeLeaders of
                Just leaders -> do
                    putBatchBi $ putLeadersForEpochSeparatelyOps i leaders
                    initLeaders (i + 1)
                Nothing -> pure ()

isLrcDbInitialized :: MonadDB m => m Bool
isLrcDbInitialized = dbHasKey lrcDbInitFlag

putInitFlag :: MonadDB m => m ()
putInitFlag = putBi lrcDbInitFlag ()

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersForEpochKey :: EpochIndex -> ByteString
leadersForEpochKey = mappend "l/" . serialize'

leaderKey :: HasProtocolConstants => SlotId -> ByteString
leaderKey = mappend "ls/" . serialize' . flattenSlotId

lrcDbInitFlag :: ByteString
lrcDbInitFlag = "linit/"

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

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
