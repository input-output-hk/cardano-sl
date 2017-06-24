{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of GState DB which stores data necessary for update system.

module Pos.Update.DB
       (
         -- * Getters
         getAdoptedBV
       , getAdoptedBVData
       , getAdoptedBVFull
       , getBVState
       , getProposalState
       , getConfirmedSV
       , getMaxBlockSize
       , getSlottingData
       , getEpochProposers

         -- * Operations
       , UpdateOp (..)

         -- * Initialization
       , initGStateUS

        -- * Iteration and related getters
       , PropIter
       , getProposalsByApp
       , getOldProposals
       , getDeepProposals

       , ConfPropIter
       , getConfirmedProposals

       , BVIter
       , getProposedBVs
       , getCompetingBVStates
       , getProposedBVStates
       ) where

import           Universum

import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit                 (Source, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List            as CL
import           Data.Time.Units              (convertUnit)
import qualified Database.RocksDB             as Rocks
import           Serokell.Data.Memory.Units   (Byte)

import           Pos.Binary.Class             (encodeStrict)
import           Pos.Binary.Infra.Slotting    ()
import           Pos.Binary.Update            ()
import           Pos.Core                     (ApplicationName, BlockVersion,
                                               ChainDifficulty, NumSoftwareVersion,
                                               SlotId, SoftwareVersion (..),
                                               StakeholderId, Timestamp (..))
import           Pos.Core.Constants           (epochSlots, genesisBlockVersionData,
                                               genesisSlotDuration)
import           Pos.Crypto                   (hash)
import           Pos.DB                       (DBIteratorClass (..), DBTag (..), IterType,
                                               MonadDB, MonadDBRead (..),
                                               RocksBatchOp (..), encodeWithKeyPrefix)
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.GState.Common         (gsGetBi, writeBatchGState)
import           Pos.Slotting.Types           (EpochSlottingData (..), SlottingData (..))
import           Pos.Update.Constants         (genesisBlockVersion,
                                               genesisSoftwareVersions, ourAppName)
import           Pos.Update.Core              (BlockVersionData (..), UpId,
                                               UpdateProposal (..))
import           Pos.Update.Poll.Types        (BlockVersionState (..),
                                               ConfirmedProposalState,
                                               DecidedProposalState (dpsDifficulty),
                                               ProposalState (..),
                                               UndecidedProposalState (upsSlot),
                                               cpsSoftwareVersion, psProposal)
import           Pos.Util.Util                (maybeThrow)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get last adopted block version.
getAdoptedBV :: MonadDBRead m => m BlockVersion
getAdoptedBV = fst <$> getAdoptedBVFull

-- | Get state of last adopted BlockVersion.
getAdoptedBVData :: MonadDBRead m => m BlockVersionData
getAdoptedBVData = snd <$> getAdoptedBVFull

-- | Get last adopted BlockVersion and data associated with it.
getAdoptedBVFull :: MonadDBRead m => m (BlockVersion, BlockVersionData)
getAdoptedBVFull = maybeThrow (DBMalformed msg) =<< getAdoptedBVFullMaybe
  where
    msg =
        "Update System part of GState DB is not initialized (last adopted BV is missing)"

-- | Get maximum block size (in bytes).
getMaxBlockSize :: MonadDBRead m => m Byte
getMaxBlockSize = bvdMaxBlockSize <$> getAdoptedBVData

-- | Get 'BlockVersionState' associated with given BlockVersion.
getBVState :: MonadDBRead m => BlockVersion -> m (Maybe BlockVersionState)
getBVState = gsGetBi . bvStateKey

-- | Get state of UpdateProposal for given UpId
getProposalState :: MonadDBRead m => UpId -> m (Maybe ProposalState)
getProposalState = gsGetBi . proposalKey

-- | Get last confirmed SoftwareVersion of given application.
getConfirmedSV :: MonadDBRead m => ApplicationName -> m (Maybe NumSoftwareVersion)
getConfirmedSV = gsGetBi . confirmedVersionKey

-- | Get most recent 'SlottingData'.
getSlottingData :: MonadDBRead m => m SlottingData
getSlottingData = maybeThrow (DBMalformed msg) =<< gsGetBi slottingDataKey
  where
    msg =
        "Update System part of GState DB is not initialized (slotting data is missing)"

-- | Get proposers for current epoch.
getEpochProposers :: MonadDBRead m => m (HashSet StakeholderId)
getEpochProposers = maybeThrow (DBMalformed msg) =<< gsGetBi epochProposersKey
  where
    msg =
        "Update System part of GState DB is not initialized (epoch proposers are missing)"

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

data UpdateOp
    = PutProposal !ProposalState
    | DeleteProposal !UpId
    | ConfirmVersion !SoftwareVersion
    | DelConfirmedVersion !ApplicationName
    | AddConfirmedProposal !ConfirmedProposalState
    | DelConfirmedProposal !SoftwareVersion
    | SetAdopted !BlockVersion BlockVersionData
    | SetBVState !BlockVersion !BlockVersionState
    | DelBV !BlockVersion
    | PutSlottingData !SlottingData
    | PutEpochProposers !(HashSet StakeholderId)

instance RocksBatchOp UpdateOp where
    toBatchOp (PutProposal ps) =
        [ Rocks.Put (proposalKey upId) (encodeStrict ps)]
      where
        up = psProposal ps
        upId = hash up
    toBatchOp (DeleteProposal upId) =
        [Rocks.Del (proposalKey upId)]
    toBatchOp (ConfirmVersion sv) =
        [Rocks.Put (confirmedVersionKey $ svAppName sv) (encodeStrict $ svNumber sv)]
    toBatchOp (DelConfirmedVersion app) =
        [Rocks.Del (confirmedVersionKey app)]
    toBatchOp (AddConfirmedProposal cps) =
        [Rocks.Put (confirmedProposalKey cps) (encodeStrict cps)]
    toBatchOp (DelConfirmedProposal sv) =
        [Rocks.Del (confirmedProposalKeySV sv)]
    toBatchOp (SetAdopted bv bvd) =
        [Rocks.Put adoptedBVKey (encodeStrict (bv, bvd))]
    toBatchOp (SetBVState bv st) =
        [Rocks.Put (bvStateKey bv) (encodeStrict st)]
    toBatchOp (DelBV bv) =
        [Rocks.Del (bvStateKey bv)]
    toBatchOp (PutSlottingData sd) =
        [Rocks.Put slottingDataKey (encodeStrict sd)]
    toBatchOp (PutEpochProposers proposers) =
        [Rocks.Put epochProposersKey (encodeStrict proposers)]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initGStateUS :: (MonadDB m) => Timestamp -> m ()
initGStateUS systemStart = do
    let genesisSlottingData = SlottingData
            { sdPenult      = esdPenult
            , sdPenultEpoch = 0
            , sdLast        = esdLast
            }
        esdPenult = EpochSlottingData
            { esdSlotDuration = genesisSlotDuration
            , esdStart        = systemStart
            }
        epoch1Start =
            systemStart +
            epochSlots * Timestamp (convertUnit genesisSlotDuration)
        esdLast = EpochSlottingData
            { esdSlotDuration = genesisSlotDuration
            , esdStart        = epoch1Start
            }
    writeBatchGState $
        PutSlottingData genesisSlottingData :
        PutEpochProposers mempty :
        SetAdopted genesisBlockVersion genesisBlockVersionData :
        map ConfirmVersion genesisSoftwareVersions

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data PropIter

instance DBIteratorClass PropIter where
    type IterKey PropIter = UpId
    type IterValue PropIter = ProposalState
    iterKeyPrefix = iterationPrefix

proposalSource :: (MonadDBRead m) => Source (ResourceT m) (IterType PropIter)
proposalSource = dbIterSource GStateDB (Proxy @PropIter)

-- TODO: it can be optimized by storing some index sorted by
-- 'SlotId's, but I don't think it may be crucial.
-- | Get all proposals which were issued no later than given slot.
getOldProposals
    :: MonadDBRead m
    => SlotId -> m [UndecidedProposalState]
getOldProposals slotId =
    runConduitRes $ mapOutput snd proposalSource .| CL.mapMaybe isOld .| CL.consume
  where
    isOld (PSUndecided u) | upsSlot u <= slotId = Just u
    isOld _               = Nothing

-- | Get all decided proposals which were accepted deeper than given
-- difficulty.
getDeepProposals
    :: MonadDBRead m
    => ChainDifficulty -> m [DecidedProposalState]
getDeepProposals cd =
    runConduitRes $ mapOutput snd proposalSource .| CL.mapMaybe isDeep .| CL.consume
  where
    isDeep e | PSDecided u <- e
             , Just proposalDifficulty <- dpsDifficulty u
             , proposalDifficulty <= cd = Just u
    isDeep _                 = Nothing

-- | Get states of all active 'UpdateProposal's for given 'ApplicationName'.
getProposalsByApp :: MonadDBRead m => ApplicationName -> m [ProposalState]
getProposalsByApp appName =
    runConduitRes $ mapOutput snd proposalSource .| CL.filter matchesName .| CL.consume
  where
    matchesName e = appName == (svAppName $ upSoftwareVersion $ psProposal e)

-- Iterator by confirmed proposals
data ConfPropIter

instance DBIteratorClass ConfPropIter where
    type IterKey ConfPropIter = SoftwareVersion
    type IterValue ConfPropIter = ConfirmedProposalState
    iterKeyPrefix = confirmedIterationPrefix

-- | Get confirmed proposals which update our application and have
-- version bigger than argument (or all proposals if 'Nothing' is
-- passed). For instance, current software version can be passed to
-- this function to get all proposals with bigger version.
getConfirmedProposals
    :: MonadDBRead m
    => Maybe NumSoftwareVersion -> m [ConfirmedProposalState]
getConfirmedProposals reqNsv =
    runConduitRes $
        dbIterSource GStateDB (Proxy @ConfPropIter) .|
        CL.mapMaybe onItem .|
        CL.consume
  where
    onItem (SoftwareVersion {..}, cps) =
        case reqNsv of
            Nothing -> Just cps
            Just v | svAppName == ourAppName && svNumber > v -> Just cps
                   | otherwise -> Nothing

-- Iterator by block versions
data BVIter

instance DBIteratorClass BVIter where
    type IterKey BVIter = BlockVersion
    type IterValue BVIter = BlockVersionState
    iterKeyPrefix = bvStateIterationPrefix

bvSource :: (MonadDBRead m) => Source (ResourceT m) (IterType BVIter)
bvSource = dbIterSource GStateDB (Proxy @BVIter)

-- | Get all proposed 'BlockVersion's.
getProposedBVs :: MonadDBRead m => m [BlockVersion]
getProposedBVs = runConduitRes $ mapOutput fst bvSource .| CL.consume

getProposedBVStates :: MonadDBRead m => m [BlockVersionState]
getProposedBVStates = runConduitRes $ mapOutput snd bvSource .| CL.consume

-- | Get all competing 'BlockVersion's and their states.
getCompetingBVStates
    :: MonadDBRead m
    => m [(BlockVersion, BlockVersionState)]
getCompetingBVStates =
    runConduitRes $ bvSource .| CL.filter (bvsIsConfirmed . snd) .| CL.consume

----------------------------------------------------------------------------
-- Keys ('us' prefix stands for Update System)
----------------------------------------------------------------------------

adoptedBVKey :: ByteString
adoptedBVKey = "us/adopted-block-version/"

bvStateKey :: BlockVersion -> ByteString
bvStateKey = encodeWithKeyPrefix @BVIter

bvStateIterationPrefix :: ByteString
bvStateIterationPrefix = "us/bvs/"

proposalKey :: UpId -> ByteString
proposalKey = encodeWithKeyPrefix @PropIter

confirmedVersionKey :: ApplicationName -> ByteString
confirmedVersionKey = mappend "us/cv/" . encodeStrict

iterationPrefix :: ByteString
iterationPrefix = "us/p/"

confirmedProposalKey :: ConfirmedProposalState -> ByteString
confirmedProposalKey = encodeWithKeyPrefix @ConfPropIter . cpsSoftwareVersion

confirmedProposalKeySV :: SoftwareVersion -> ByteString
confirmedProposalKeySV = encodeWithKeyPrefix @ConfPropIter

confirmedIterationPrefix :: ByteString
confirmedIterationPrefix = "us/cp/"

slottingDataKey :: ByteString
slottingDataKey = "us/slotting/"

epochProposersKey :: ByteString
epochProposersKey = "us/epoch-proposers/"

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getAdoptedBVFullMaybe
    :: MonadDBRead m
    => m (Maybe (BlockVersion, BlockVersionData))
getAdoptedBVFullMaybe = gsGetBi adoptedBVKey
