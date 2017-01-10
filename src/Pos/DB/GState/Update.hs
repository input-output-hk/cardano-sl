{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Part of GState DB which stores data necessary for update system.

module Pos.DB.GState.Update
       (
         -- * Getters
         getLastPV
       , getScriptVersion
       , getProposalState
       , getAppProposal
       , getProposalStateByApp
       , getConfirmedSV

         -- * Operations
       , UpdateOp (..)
       , setScriptVersion
       , setConfirmedSV
       , setLastPV

         -- * Initialization
       , prepareGStateUS

         -- * Iteration
       -- , getOldProposals
       ) where

import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Database.RocksDB          as Rocks
import           Universum

import           Pos.Binary.Class          (encodeStrict)
import           Pos.Binary.DB             ()
import           Pos.Constants             (curSoftwareVersion)
import           Pos.Crypto                (hash)
import           Pos.DB.Class              (MonadDB)
import           Pos.DB.Error              (DBError (DBMalformed))
import           Pos.DB.Functions          (RocksBatchOp (..))
import           Pos.DB.GState.Common      (getBi, putBi)
import           Pos.DB.Types              (ProposalState (..), psProposal)
import           Pos.Genesis               (genesisProtocolVersion, genesisScriptVersion)
import           Pos.Script.Type           (ScriptVersion)
import           Pos.Types                 (ApplicationName, ProtocolVersion,
                                            SoftwareVersion (..))
import           Pos.Update.Types          (UpId, UpdateProposal (..))
import           Pos.Util                  (maybeThrow)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get last approved protocol version.
getLastPV :: MonadDB ssc m => m ProtocolVersion
getLastPV = maybeThrow (DBMalformed msg) =<< getLastPVMaybe
  where
    msg =
        "Update System part of GState DB is not initialized (last PV is missing)"

getScriptVersion :: MonadDB ssc m => ProtocolVersion -> m (Maybe ScriptVersion)
getScriptVersion = getBi . scriptVersionKey

-- | Get state of UpdateProposal for given UpId
getProposalState :: MonadDB ssc m => UpId -> m (Maybe ProposalState)
getProposalState = getBi . proposalKey

-- | Get UpId of current proposal for given appName
getAppProposal :: MonadDB ssc m => ApplicationName -> m (Maybe UpId)
getAppProposal = getBi . proposalAppKey

-- | Get state of Update Proposal for given AppName
getProposalStateByApp :: MonadDB ssc m => ApplicationName -> m (Maybe ProposalState)
getProposalStateByApp appName =
    runMaybeT $ MaybeT (getAppProposal appName) >>= MaybeT . getProposalState

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
    | DeleteProposal !UpId !ApplicationName

instance RocksBatchOp UpdateOp where
    toBatchOp (PutProposal ps) =
        [ Rocks.Put (proposalKey upId) (encodeStrict ps)
        , Rocks.Put (proposalAppKey appName) (encodeStrict upId)
        ]
      where
        up = psProposal ps
        upId = hash up
        appName = svAppName $ upSoftwareVersion up
    toBatchOp (DeleteProposal upId appName) =
        [Rocks.Del (proposalAppKey appName), Rocks.Del (proposalKey upId)]

-- putUndecidedProposalSlot :: ProposalState -> [Rocks.BatchOp]
-- putUndecidedProposalSlot (PSUndecided ups) =
--     [Rocks.Put (proposalAppKey (upsSlot ups)) (encodeStrict (upsProposal ups))]
-- putUndecidedProposalSlot _ = []

-- | Set last protocol version
setLastPV :: MonadDB ssc m => ProtocolVersion -> m ()
setLastPV = putBi lastPVKey

-- | Set correspondence between protocol version and script version
setScriptVersion :: MonadDB ssc m => ProtocolVersion -> ScriptVersion -> m ()
setScriptVersion = putBi . scriptVersionKey

-- | Set confirmed version number for given app
setConfirmedSV :: MonadDB ssc m => SoftwareVersion -> m ()
setConfirmedSV SoftwareVersion {..}
    = putBi (confirmedVersionKey svAppName) svNumber

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateUS
    :: forall ssc m.
       MonadDB ssc m
    => m ()
prepareGStateUS = unlessM isInitialized $ do
    setScriptVersion genesisProtocolVersion genesisScriptVersion
    setConfirmedSV curSoftwareVersion
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
-- getOldProposals
--     :: forall ssc m.
--        (MonadDB ssc m, MonadMask m)
--     => SlotId -> m [ProposalState]
-- getOldProposals slotId = do
--     db <- getUtxoDB
--     traverseAllEntries db (pure []) step
--   where
--     msg = "proposal for version associated with slot is not found"
--     step :: [ProposalState] -> SlotId -> SoftwareVersion -> m [ProposalState]
--     step res storedSlotId sw
--         | storedSlotId > slotId = pure res
--         | otherwise = do
--             ps <- maybeThrow (DBMalformed msg) =<< getProposalState sw
--             return $ ps : res

----------------------------------------------------------------------------
-- Keys ('us' prefix stands for Update System)
----------------------------------------------------------------------------

lastPVKey :: ByteString
lastPVKey = "us/last-protocol"

scriptVersionKey :: ProtocolVersion -> ByteString
scriptVersionKey = mappend "us/vs" . encodeStrict

proposalKey :: UpId -> ByteString
proposalKey = mappend "us/p" . encodeStrict

proposalAppKey :: ApplicationName -> ByteString
proposalAppKey = mappend "us/an" . encodeStrict

confirmedVersionKey :: ApplicationName -> ByteString
confirmedVersionKey = mappend "us/c" . encodeStrict

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getLastPVMaybe :: MonadDB ssc m => m (Maybe ProtocolVersion)
getLastPVMaybe = getBi lastPVKey
