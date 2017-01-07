{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Part of GState DB which stores data necessary for update system.

module Pos.DB.GState.Update
       (
         -- * Getters
         getLastPV
       , getProposalState
       , getStakeUS
       , getConfirmedSV

         -- * Operations
       , UpdateOp (..)
       , putRichmenUS

         -- * Initialization
       , prepareGStateUS

         -- * Iteration
       , getOldProposals
       ) where

import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.Binary.Class     (encodeStrict)
import           Pos.Binary.DB        ()
import           Pos.DB.Class         (MonadDB, getUtxoDB)
import           Pos.DB.Error         (DBError (DBMalformed))
import           Pos.DB.Functions     (RocksBatchOp (..), traverseAllEntries)
import           Pos.DB.GState.Common (getBi, putBi)
import           Pos.DB.Types         (ProposalState (..), UndecidedProposalState (..),
                                       psProposal)
import           Pos.Genesis          (genesisProtocolVersion)
import           Pos.Types            (ApplicationName, Coin, EpochIndex, ProtocolVersion,
                                       SlotId, SoftwareVersion (..), StakeholderId,
                                       UpdateProposal (..))
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

-- | Get stake of richmen according to stake distribution for given
-- epoch. If stakeholder was not richmen or if richmen for epoch are
-- unknown, 'Nothing' is returned.
getStakeUS :: MonadDB ssc m => EpochIndex -> StakeholderId -> m (Maybe Coin)
getStakeUS e = getBi . stakeKey e

type NumSoftwareVersion = Word32

-- | Get last confirmed SoftwareVersion of given application.
getConfirmedSV
    :: MonadDB ssc m
    => ApplicationName -> m (Maybe NumSoftwareVersion)
getConfirmedSV = getBi . confirmedVersionKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

data UpdateOp
    = PutProposal !ProposalState
    | DeleteProposal !SlotId !SoftwareVersion
    | ConfirmUpdate !SoftwareVersion

instance RocksBatchOp UpdateOp where
    toBatchOp (PutProposal ps) =
        Rocks.Put (proposalKey (upSoftwareVersion up)) (encodeStrict up) :
        putUndecidedProposalSlot ps
      where
        up = psProposal ps
    toBatchOp (DeleteProposal slotId sv) =
        [Rocks.Del (proposalSlotKey slotId), Rocks.Del (proposalKey sv)]
    toBatchOp (ConfirmUpdate SoftwareVersion {..}) =
        [Rocks.Put (confirmedVersionKey svAppName) (encodeStrict svNumber)]

putUndecidedProposalSlot :: ProposalState -> [Rocks.BatchOp]
putUndecidedProposalSlot (PSUndecided ups) =
    [Rocks.Put (proposalSlotKey (upsSlot ups)) (encodeStrict (upsProposal ups))]
putUndecidedProposalSlot _ = []

-- I suppose batching is not necessary here.
-- | Put richmen and their stakes for given epoch.
putRichmenUS :: MonadDB ssc m => EpochIndex -> [(StakeholderId, Coin)] -> m ()
putRichmenUS e = mapM_ putRichmenDo
  where
    putRichmenDo (id, stake) = putBi (stakeKey e id) stake

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

-- Can be optimized I suppose.
stakeKey :: EpochIndex -> StakeholderId -> ByteString
stakeKey e s = "us/s" <> encodeStrict e <> encodeStrict s

confirmedVersionKey :: ApplicationName -> ByteString
confirmedVersionKey = mappend "us/c" . encodeStrict

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getLastPVMaybe :: MonadDB ssc m => m (Maybe ProtocolVersion)
getLastPVMaybe = getBi lastPVKey
