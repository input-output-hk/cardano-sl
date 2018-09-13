{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Part of GState DB which stores data necessary for update system.

module Pos.DB.Update.GState
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

import           Control.Lens (at)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit (ConduitT, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import           Data.Time.Units (Microsecond, convertUnit)
import qualified Database.RocksDB as Rocks
import           Serokell.Data.Memory.Units (Byte)
import           UnliftIO (MonadUnliftIO)

import           Pos.Binary.Class (serialize')
import           Pos.Chain.Update (BlockVersionState (..),
                     ConfirmedProposalState (..),
                     DecidedProposalState (dpsDifficulty),
                     HasUpdateConfiguration, ProposalState (..),
                     UndecidedProposalState (upsSlot), bvsIsConfirmed,
                     cpsSoftwareVersion, genesisBlockVersion,
                     genesisSoftwareVersions, ourAppName, ourSystemTag,
                     psProposal)
import           Pos.Core as Core (ChainDifficulty, Config (..), SlotId,
                     StakeholderId, TimeDiff (..), configBlockVersionData,
                     configEpochSlots)
import           Pos.Core.Slotting (EpochSlottingData (..), SlottingData,
                     createInitSlottingData)
import           Pos.Core.Update (ApplicationName, BlockVersion,
                     BlockVersionData (..), NumSoftwareVersion,
                     SoftwareVersion (..), UpId, UpdateProposal (..))
import           Pos.Crypto (hash)
import           Pos.DB (DBIteratorClass (..), DBTag (..), IterType, MonadDB,
                     MonadDBRead (..), RocksBatchOp (..), encodeWithKeyPrefix)
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.GState.Common (gsGetBi, writeBatchGState)
import           Pos.Util.Util (maybeThrow)

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
getProposalState :: (MonadDBRead m) => UpId -> m (Maybe ProposalState)
getProposalState = gsGetBi . proposalKey

-- | Get last confirmed SoftwareVersion of given application.
getConfirmedSV :: MonadDBRead m => ApplicationName -> m (Maybe NumSoftwareVersion)
getConfirmedSV = gsGetBi . confirmedVersionKey

-- | Get most recent 'SlottingData'.
getSlottingData :: MonadDBRead m => m SlottingData
getSlottingData = maybeThrow (DBMalformed msg) =<< gsGetBi slottingDataKey
  where
    msg = "Update System part of GState DB is not initialized (slotting data is missing)"

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
        [ Rocks.Put (proposalKey upId) (serialize' ps)]
      where
        up = psProposal ps
        upId = hash up
    toBatchOp (DeleteProposal upId) =
        [Rocks.Del (proposalKey upId)]
    toBatchOp (ConfirmVersion sv) =
        [Rocks.Put (confirmedVersionKey $ svAppName sv) (serialize' $ svNumber sv)]
    toBatchOp (DelConfirmedVersion app) =
        [Rocks.Del (confirmedVersionKey app)]
    toBatchOp (AddConfirmedProposal cps) =
        [Rocks.Put (confirmedProposalKey cps) (serialize' cps)]
    toBatchOp (DelConfirmedProposal sv) =
        [Rocks.Del (confirmedProposalKeySV sv)]
    toBatchOp (SetAdopted bv bvd) =
        [Rocks.Put adoptedBVKey (serialize' (bv, bvd))]
    toBatchOp (SetBVState bv st) =
        [Rocks.Put (bvStateKey bv) (serialize' st)]
    toBatchOp (DelBV bv) =
        [Rocks.Del (bvStateKey bv)]
    toBatchOp (PutSlottingData sd) =
        [Rocks.Put slottingDataKey (serialize' sd)]
    toBatchOp (PutEpochProposers proposers) =
        [Rocks.Put epochProposersKey (serialize' proposers)]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initGStateUS :: MonadDB m => Core.Config -> m ()
initGStateUS coreConfig = do
    writeBatchGState $
        PutSlottingData genesisSlottingData :
        PutEpochProposers mempty :
        SetAdopted genesisBlockVersion genesisBvd :
        map ConfirmVersion genesisSoftwareVersions
  where
    genesisBvd = configBlockVersionData coreConfig
    genesisSlotDuration = bvdSlotDuration genesisBvd

    genesisEpochDuration :: Microsecond
    genesisEpochDuration = fromIntegral (configEpochSlots coreConfig) * convertUnit genesisSlotDuration

    esdCurrent :: EpochSlottingData
    esdCurrent = EpochSlottingData
        { esdSlotDuration = genesisSlotDuration
        , esdStartDiff    = 0
        }

    esdNext :: EpochSlottingData
    esdNext = EpochSlottingData
        { esdSlotDuration = genesisSlotDuration
        , esdStartDiff    = TimeDiff genesisEpochDuration
        }

    genesisSlottingData :: SlottingData
    genesisSlottingData = createInitSlottingData esdCurrent esdNext

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data PropIter

instance DBIteratorClass PropIter where
    type IterKey PropIter = UpId
    type IterValue PropIter = ProposalState
    iterKeyPrefix = iterationPrefix

proposalSource ::
       (MonadDBRead m)
    => ConduitT () (IterType PropIter) (ResourceT m) ()
proposalSource = dbIterSource GStateDB (Proxy @PropIter)

-- TODO: it can be optimized by storing some index sorted by
-- 'SlotId's, but I don't think it may be crucial.
-- | Get all proposals which were issued no later than given slot.
getOldProposals
    :: (MonadDBRead m, MonadUnliftIO m)
    => SlotId -> m [UndecidedProposalState]
getOldProposals slotId =
    runConduitRes $ mapOutput snd proposalSource .| CL.mapMaybe isOld .| CL.consume
  where
    isOld (PSUndecided u) | upsSlot u <= slotId = Just u
    isOld _               = Nothing

-- | Get all decided proposals which were accepted deeper than given
-- difficulty.
getDeepProposals
    :: (MonadDBRead m, MonadUnliftIO m)
    => ChainDifficulty -> m [DecidedProposalState]
getDeepProposals cd =
    runConduitRes $ mapOutput snd proposalSource .| CL.mapMaybe isDeep .| CL.consume
  where
    isDeep e | PSDecided u <- e
             , Just proposalDifficulty <- dpsDifficulty u
             , proposalDifficulty <= cd = Just u
    isDeep _                 = Nothing

-- | Get states of all competing 'UpdateProposal's for given 'ApplicationName'.
getProposalsByApp ::
       (MonadDBRead m, MonadUnliftIO m)
    => ApplicationName
    -> m [ProposalState]
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

-- | Get confirmed proposals which update our application
-- (i. e. application name matches our application name and there is
-- update data for our system tag) and have version greater than
-- argument. Intended usage is to pass numberic version of this
-- software as argument.
-- Returns __all__ confirmed proposals if the argument is 'Nothing'.
getConfirmedProposals
    :: (HasUpdateConfiguration, MonadDBRead m, MonadUnliftIO m)
    => Maybe NumSoftwareVersion -> m [ConfirmedProposalState]
getConfirmedProposals reqNsv =
    runConduitRes $
    dbIterSource GStateDB (Proxy @ConfPropIter) .| CL.mapMaybe onItem .|
    CL.consume
  where
    onItem (SoftwareVersion {..}, cps)
        | Nothing <- reqNsv = Just cps
        | Just v <- reqNsv
        , hasOurSystemTag cps && svAppName == ourAppName && svNumber > v =
            Just cps
        | otherwise = Nothing
    hasOurSystemTag ConfirmedProposalState {..} =
        isJust $ upData cpsUpdateProposal ^. at ourSystemTag

-- Iterator by block versions
data BVIter

instance DBIteratorClass BVIter where
    type IterKey BVIter = BlockVersion
    type IterValue BVIter = BlockVersionState
    iterKeyPrefix = bvStateIterationPrefix

bvSource :: (MonadDBRead m) => ConduitT () (IterType BVIter) (ResourceT m) ()
bvSource = dbIterSource GStateDB (Proxy @BVIter)

-- | Get all proposed 'BlockVersion's.
getProposedBVs :: (MonadDBRead m, MonadUnliftIO m) => m [BlockVersion]
getProposedBVs = runConduitRes $ mapOutput fst bvSource .| CL.consume

getProposedBVStates :: (MonadDBRead m, MonadUnliftIO m) => m [BlockVersionState]
getProposedBVStates = runConduitRes $ mapOutput snd bvSource .| CL.consume

-- | Get all competing 'BlockVersion's and their states.
getCompetingBVStates
    :: (MonadDBRead m, MonadUnliftIO m)
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
confirmedVersionKey = mappend "us/cv/" . serialize'

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
