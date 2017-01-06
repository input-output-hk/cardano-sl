{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Part of GState DB which stores data necessary for update system.

module Pos.DB.GState.Update
       (
         -- * Getters
         getLastPV
       , getProposalState

         -- * Initialization
       , prepareGStateUS

         -- * Iteration
       , getOldProposals
       ) where

import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Binary.DB        ()
import           Pos.DB.Class         (MonadDB, getUtxoDB)
import           Pos.DB.Error         (DBError (DBMalformed))
import           Pos.DB.Functions     (traverseAllEntries)
import           Pos.DB.GState.Common (getBi, putBi)
import           Pos.DB.Types         (ProposalState)
import           Pos.Genesis          (genesisProtocolVersion)
import           Pos.Types            (ProtocolVersion, SlotId, SoftwareVersion)
import           Pos.Util             (maybeThrow)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get last approved protocol version.
getLastPV :: MonadDB ssc m => m ProtocolVersion
getLastPV = maybeThrow (DBMalformed msg) =<< getLastPVMaybe
  where
    msg =
        "Update System part of GState DB is not initialized (last PV is missing)"

-- | Get state of UpdateProposal for given SoftwareVersion.
getProposalState :: MonadDB ssc m => SoftwareVersion -> m (Maybe ProposalState)
getProposalState = getBi . proposalKey

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateUS
    :: forall ssc m.
       MonadDB ssc m
    => m ()
prepareGStateUS = unlessM isInitialized $ do
    putBi lastPVKey genesisProtocolVersion

isInitialized :: MonadDB ssc m => m Bool
isInitialized = isJust <$> getLastPVMaybe

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- TODO!
-- I don't like it at all!
-- 1. Idea is to iterate in sorted order and stop early.
-- 2. I suppose this logic should be outside, this module should provide only
-- basic functionality, but it's not convenient currently.
-- 3. Also I am not sure that having all proposals in memory is good idea here.
-- It is definitly not necessary. But we never have time to write good code, so
-- it's the only option now.
-- | Get all proposals which were issued no later than given slot.
-- Head is youngest proposal.
getOldProposals
    :: forall ssc m.
       (MonadDB ssc m, MonadMask m)
    => SlotId -> m [ProposalState]
getOldProposals slotId = do
    db <- getUtxoDB
    traverseAllEntries db (pure []) step
  where
    msg = "proposal for version associated with slot is not found"
    step :: [ProposalState] -> SlotId -> SoftwareVersion -> m [ProposalState]
    step res storedSlotId sw
        | storedSlotId > slotId = pure res
        | otherwise = do
            ps <- maybeThrow (DBMalformed msg) =<< getProposalState sw
            return $ ps : res

----------------------------------------------------------------------------
-- Keys ('us' prefix stands for Update System)
----------------------------------------------------------------------------

lastPVKey :: ByteString
lastPVKey = "us/last-protocol"

proposalKey :: SoftwareVersion -> ByteString
proposalKey = mappend "us/p" . encodeStrict

proposalSlotKey :: SlotId -> ByteString
-- [CSL-379] Restore prefix after we have proper iterator
-- proposalSlotKey = mappend "us/s" . encodeStrict
proposalSlotKey = encodeStrict

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getLastPVMaybe :: MonadDB ssc m => m (Maybe ProtocolVersion)
getLastPVMaybe = getBi lastPVKey
