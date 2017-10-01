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
import           Pos.DB.Class          (MonadDB (dbWriteBatch), MonadDBRead)
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

putLeadersForEpoch :: MonadDB m => EpochIndex -> SlotLeaders -> m ()
putLeadersForEpoch epoch leaders = do
    putBi (leadersForEpochKey epoch) leaders
    putBatchBi putLeadersForEachSlotOps
  where
    putLeadersForEachSlotOps :: [(ByteString, StakeholderId)]
    putLeadersForEachSlotOps = [
            (leaderKey $ mkSlotId epoch i, leader)
            | (i, leader) <- zip [1..] $ toList leaders
        ]
    mkSlotId :: EpochIndex -> Word16 -> SlotId
    mkSlotId epoch slot =
        -- Using @UnsafeLocalSlotIndex@ because we trust the callers.
        SlotId epoch (UnsafeLocalSlotIndex slot)

putLeader :: MonadDB m => SlotId -> StakeholderId -> m ()
putLeader slot stakeholderId = putBi (leaderKey slot) stakeholderId

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcLeaders ::
       ( MonadDB m
       , HasConfiguration
       )
    => m ()
prepareLrcLeaders =
    whenNothingM_ (getLeadersForEpoch 0) $
        putLeadersForEpoch 0 genesisLeaders

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersForEpochKey :: EpochIndex -> ByteString
leadersForEpochKey = mappend "l/" . serialize'

leaderKey :: HasProtocolConstants => SlotId -> ByteString
leaderKey = mappend "ls/" . serialize' . flattenSlotId
