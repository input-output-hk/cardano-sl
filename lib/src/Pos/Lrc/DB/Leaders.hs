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
                                        HasProtocolConstants,
                                        LocalSlotIndex (UnsafeLocalSlotIndex),
                                        SlotId (SlotId), SlotLeaders, StakeholderId,
                                        flattenSlotId)
import           Pos.DB.Class          (MonadDB, MonadDBRead)
import           Pos.Lrc.DB.Common     (getBi, putBatchBi, putBi)

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
    putLeadersForEpochAllAtOnce epoch leaders
    putLeadersForEpochSeparately epoch leaders

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
    whenNothingM_ (getLeadersForEpoch 0) $
        putLeadersForEpoch 0 genesisLeaders
    -- Smooth migration for CSE-240.
    initLeaders 1
  where
    initLeaders :: MonadDB m => EpochIndex -> m ()
    initLeaders i = do
        whenNothingM_ (getLeader $ SlotId i (UnsafeLocalSlotIndex 0)) $ do
            maybeLeaders <- getLeadersForEpoch i
            case maybeLeaders of
                Just leaders -> do
                    putLeadersForEpochSeparately i leaders
                    initLeaders (i + 1)
                Nothing -> pure ()

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersForEpochKey :: EpochIndex -> ByteString
leadersForEpochKey = mappend "l/" . serialize'

leaderKey :: HasProtocolConstants => SlotId -> ByteString
leaderKey = mappend "ls/" . serialize' . flattenSlotId

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

putLeadersForEpochAllAtOnce :: MonadDB m => EpochIndex -> SlotLeaders -> m ()
putLeadersForEpochAllAtOnce epoch = putBi (leadersForEpochKey epoch)

putLeadersForEpochSeparately :: MonadDB m => EpochIndex -> SlotLeaders -> m ()
putLeadersForEpochSeparately epoch leaders =
    putBatchBi putLeadersForEachSlotOps
  where
    putLeadersForEachSlotOps :: [(ByteString, StakeholderId)]
    putLeadersForEachSlotOps = [
            (leaderKey $ mkSlotId epoch i, leader)
            | (i, leader) <- zip [1..] $ toList leaders
        ]
    mkSlotId :: EpochIndex -> Word16 -> SlotId
    mkSlotId epoch' slot =
        -- Using @UnsafeLocalSlotIndex@ because we trust the callers.
        SlotId epoch' (UnsafeLocalSlotIndex slot)
