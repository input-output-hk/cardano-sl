{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for update system.

module Pos.DB.GState.Update
       (
         -- * Getters
         getLastAdoptedBV
       , getLastScriptVersion
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

       , ConfPropIter
       , getConfirmedProposals
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
import           Pos.Genesis               (genesisBlockVersion, genesisScriptVersion,
                                            genesisSoftwareVersions)
import           Pos.Script.Type           (ScriptVersion)
import           Pos.Types                 (ApplicationName, BlockVersion,
                                            ChainDifficulty, NumSoftwareVersion, SlotId,
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

-- | Get last adopted block version.
getLastAdoptedBV :: MonadDB ssc m => m BlockVersion
getLastAdoptedBV = maybeThrow (DBMalformed msg) =<< getLastAdoptedBVMaybe
  where
    msg =
        "Update System part of GState DB is not initialized (last adopted BV is missing)"

getLastScriptVersion :: MonadDB ssc m => m ScriptVersion
getLastScriptVersion = do
    lbv <- getLastAdoptedBV
    maybeThrow (DBMalformed msg) =<< getScriptVersion lbv
  where
    msg =
        "Update System part of GState DB : Last Script Version is missing"

getScriptVersion :: MonadDB ssc m => BlockVersion -> m (Maybe ScriptVersion)
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
    | DelConfirmedVersion !ApplicationName
    | AddConfirmedProposal !NumSoftwareVersion !UpdateProposal
    | SetLastPV !BlockVersion
    | SetScriptVersion !BlockVersion !ScriptVersion
    | DelScriptVersion !BlockVersion

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
    toBatchOp (DelConfirmedVersion app) =
        [Rocks.Del (confirmedVersionKey app)]
    toBatchOp (AddConfirmedProposal nsv up) =
        [Rocks.Put (confirmedProposalKey nsv) (encodeStrict up)]
    toBatchOp (SetLastPV pv) =
        [Rocks.Put lastBVKey (encodeStrict pv)]
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
        [ SetLastPV genesisBlockVersion
        , SetScriptVersion genesisBlockVersion genesisScriptVersion
        ] <> map ConfirmVersion genesisSoftwareVersions

isInitialized :: MonadDB ssc m => m Bool
isInitialized = isJust <$> getLastAdoptedBVMaybe

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

-- Iterator by confirmed proposals
data ConfPropIter

instance DBIteratorClass ConfPropIter where
    type IterKey ConfPropIter = NumSoftwareVersion
    type IterValue ConfPropIter = UpdateProposal
    iterKeyPrefix _ = confirmedIterationPrefix

-- | Get confirmed proposals which update our application and have
-- version bigger than argument. For instance, current software
-- version can be passed to this function to get all proposals with
-- bigger version.
getConfirmedProposals :: MonadDB ssc m => NumSoftwareVersion -> m [UpdateProposal]
getConfirmedProposals reqNsv = runDBnIterator @ConfPropIter _gStateDB (step [])
  where
    step res = nextItem >>= maybe (pure res) (onItem res)
    onItem res (nsv, up)
        | nsv > reqNsv = step (up:res)
        | otherwise    = step res

----------------------------------------------------------------------------
-- Keys ('us' prefix stands for Update System)
----------------------------------------------------------------------------

lastBVKey :: ByteString
lastBVKey = "us/last-adopted-block-v"

scriptVersionKey :: BlockVersion -> ByteString
scriptVersionKey = mappend "us/vs" . encodeStrict

proposalKey :: UpId -> ByteString
proposalKey = encodeWithKeyPrefix @PropIter

proposalAppKey :: ApplicationName -> ByteString
proposalAppKey = mappend "us/an" . encodeStrict

confirmedVersionKey :: ApplicationName -> ByteString
confirmedVersionKey = mappend "us/cv" . encodeStrict

iterationPrefix :: ByteString
iterationPrefix = "us/p"

confirmedProposalKey :: NumSoftwareVersion -> ByteString
confirmedProposalKey = encodeWithKeyPrefix @ConfPropIter

confirmedIterationPrefix :: ByteString
confirmedIterationPrefix = "us/cp"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getLastAdoptedBVMaybe :: MonadDB ssc m => m (Maybe BlockVersion)
getLastAdoptedBVMaybe = getBi lastBVKey
