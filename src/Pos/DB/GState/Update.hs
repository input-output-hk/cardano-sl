{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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

         -- * Initialization
       , prepareGStateUS

        -- * Iteration
       , PropIter
       , runProposalMapIterator
       , runProposalIterator
       , getOldProposals
       , getDeepProposals
       ) where

import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Database.RocksDB          as Rocks
import           Universum

import           Pos.Binary.Class          (encodeStrict)
import           Pos.Binary.DB             ()
import           Pos.Crypto                (hash)
import           Pos.DB.Class              (MonadDB, getUtxoDB)
import           Pos.DB.Error              (DBError (DBMalformed))
import           Pos.DB.Functions          (RocksBatchOp (..), encodeWithKeyPrefix,
                                            rocksWriteBatch)
import           Pos.DB.GState.Common      (getBi)
import           Pos.DB.Iterator           (DBIteratorClass (..), DBnIterator,
                                            DBnMapIterator, IterType, runDBnIterator,
                                            runDBnMapIterator)
import           Pos.DB.Types              (NodeDBs (..))
import           Pos.Genesis               (genesisProtocolVersion, genesisScriptVersion,
                                            genesisSoftwareVersions)
import           Pos.Script.Type           (ScriptVersion)
import           Pos.Types                 (ApplicationName, ChainDifficulty,
                                            NumSoftwareVersion, ProtocolVersion, SlotId,
                                            SoftwareVersion (..))
import           Pos.Update.Core           (UpId, UpdateProposal (..))
import           Pos.Update.Poll.Types     (DecidedProposalState (dpsDifficulty),
                                            ProposalState (..),
                                            UndecidedProposalState (upsSlot), psProposal)
import           Pos.Util                  (maybeThrow)
import           Pos.Util.Iterator         (MonadIterator (..))

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
    | ConfirmVersion !SoftwareVersion
    | SetLastPV !ProtocolVersion
    | SetScriptVersion !ProtocolVersion !ScriptVersion
    | DelScriptVersion !ProtocolVersion

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
    toBatchOp (ConfirmVersion sv) =
        [Rocks.Put (confirmedVersionKey $ svAppName sv) (encodeStrict $ svNumber sv)]
    toBatchOp (SetLastPV pv) =
        [Rocks.Put lastPVKey (encodeStrict pv)]
    toBatchOp (SetScriptVersion pv sv) =
        [Rocks.Put (scriptVersionKey pv) (encodeStrict sv)]
    toBatchOp (DelScriptVersion pv) =
        [Rocks.Del (scriptVersionKey pv)]

-- putUndecidedProposalSlot :: ProposalState -> [Rocks.BatchOp]
-- putUndecidedProposalSlot (PSUndecided ups) =
--     [Rocks.Put (proposalAppKey (upsSlot ups)) (encodeStrict (upsProposal ups))]
-- putUndecidedProposalSlot _ = []

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareGStateUS
    :: forall ssc m.
       MonadDB ssc m
    => m ()
prepareGStateUS = unlessM isInitialized $ do
    db <- getUtxoDB
    flip rocksWriteBatch db $
        [ SetLastPV genesisProtocolVersion
        , SetScriptVersion genesisProtocolVersion genesisScriptVersion
        ] <> map ConfirmVersion genesisSoftwareVersions

isInitialized :: MonadDB ssc m => m Bool
isInitialized = isJust <$> getLastPVMaybe

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data PropIter

instance DBIteratorClass PropIter where
    type IterKey PropIter = UpId
    type IterValue PropIter = ProposalState
    iterKeyPrefix _ = iterationPrefix

runProposalIterator
    :: forall m ssc a . MonadDB ssc m
    => DBnIterator ssc PropIter a -> m a
runProposalIterator = runDBnIterator @PropIter _gStateDB

runProposalMapIterator
    :: forall v m ssc a . MonadDB ssc m
    => DBnMapIterator ssc PropIter v a -> (IterType PropIter -> v) -> m a
runProposalMapIterator = runDBnMapIterator @PropIter _gStateDB


-- TODO: it can be optimized by storing some index sorted by
-- 'SlotId's, but I don't think it may be crucial.
-- | Get all proposals which were issued no later than given slot.
getOldProposals
    :: forall ssc m. MonadDB ssc m
    => SlotId -> m [UndecidedProposalState]
getOldProposals slotId = runProposalMapIterator (step []) snd
  where
    step res = nextItem >>= maybe (pure res) (onItem res)
    onItem res e
        | PSUndecided u <- e
        , upsSlot u <= slotId = step (u:res)
        | otherwise = step res

-- TODO: eliminate copy-paste here!

-- | Get all decided proposals which were accepted deeper than given
-- difficulty.
getDeepProposals
    :: forall ssc m. MonadDB ssc m
    => ChainDifficulty -> m [DecidedProposalState]
getDeepProposals cd = runProposalMapIterator (step []) snd
  where
    step res = nextItem >>= maybe (pure res) (onItem res)
    onItem res e
        | PSDecided u <- e
        , Just proposalDifficulty <- dpsDifficulty u
        , proposalDifficulty <= cd = step (u : res)
        | otherwise = step res

----------------------------------------------------------------------------
-- Keys ('us' prefix stands for Update System)
----------------------------------------------------------------------------

lastPVKey :: ByteString
lastPVKey = "us/last-protocol"

scriptVersionKey :: ProtocolVersion -> ByteString
scriptVersionKey = mappend "us/vs" . encodeStrict

proposalKey :: UpId -> ByteString
proposalKey = encodeWithKeyPrefix @PropIter

proposalAppKey :: ApplicationName -> ByteString
proposalAppKey = mappend "us/an" . encodeStrict

confirmedVersionKey :: ApplicationName -> ByteString
confirmedVersionKey = mappend "us/c" . encodeStrict

iterationPrefix :: ByteString
iterationPrefix = "us/p"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getLastPVMaybe :: MonadDB ssc m => m (Maybe ProtocolVersion)
getLastPVMaybe = getBi lastPVKey

-- -- Set last protocol version
-- setLastPV :: MonadDB ssc m => ProtocolVersion -> m ()
-- setLastPV = putBi lastPVKey

-- -- Set correspondence between protocol version and script version
-- setScriptVersion :: MonadDB ssc m => ProtocolVersion -> ScriptVersion -> m ()
-- setScriptVersion = putBi . scriptVersionKey

-- -- Set confirmed version number for given app
-- setConfirmedSV :: MonadDB ssc m => SoftwareVersion -> m ()
-- setConfirmedSV SoftwareVersion {..}
--     = putBi (confirmedVersionKey svAppName) svNumber
