{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Part of GState DB which stores data necessary for update system.

module Pos.DB.Update.GState
       (
         -- * Getters and types re-exported from Pos.DB.Update.GState.BlockVersion
         getConsensusEra
       , getAdoptedBVData
       , getAdoptedBVFull
       , getAdoptedBV
       , getBVState
       , BVIter
       , getProposedBVs
       , getCompetingBVStates
       , getProposedBVStates

         -- * Getters
       , getProposalState
       , getConfirmedSV
       , getMaxBlockSize
       , getSlottingData
       , getEpochProposers
       , getAllProposals

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
import           Pos.Chain.Genesis as Genesis (Config (..),
                     configBlockVersionData, configEpochSlots)
import           Pos.Chain.Update (ApplicationName, BlockVersion,
                     BlockVersionData (..), BlockVersionState (..),
                     ConfirmedProposalState (..),
                     DecidedProposalState (dpsDifficulty), NumSoftwareVersion,
                     ProposalState (..), SoftwareVersion (..),
                     UndecidedProposalState (upsSlot), UpId,
                     UpdateConfiguration, UpdateProposal (..),
                     cpsSoftwareVersion, genesisBlockVersion,
                     genesisSoftwareVersions, ourAppName, ourSystemTag,
                     psProposal)
import           Pos.Core (ChainDifficulty, SlotId, StakeholderId,
                     TimeDiff (..))
import           Pos.Core.Slotting (EpochSlottingData (..), SlottingData,
                     createInitSlottingData)
import           Pos.Crypto (hash)
import           Pos.DB.BatchOp (RocksBatchOp (..))
import           Pos.DB.Class (DBIteratorClass (..), DBTag (..), IterType,
                     MonadDB, MonadDBRead (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (encodeWithKeyPrefix)
import           Pos.DB.GState.Common (gsGetBi, writeBatchGState)
import           Pos.DB.Update.GState.BlockVersion (BVIter, adoptedBVKey,
                     bvStateKey, getAdoptedBV, getAdoptedBVData,
                     getAdoptedBVFull, getBVState, getCompetingBVStates,
                     getConsensusEra, getProposedBVStates, getProposedBVs)
import           Pos.Util.Util (maybeThrow)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get maximum block size (in bytes).
getMaxBlockSize :: MonadDBRead m => m Byte
getMaxBlockSize = bvdMaxBlockSize <$> getAdoptedBVData

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

initGStateUS :: MonadDB m => Genesis.Config -> m ()
initGStateUS genesisConfig = do
    writeBatchGState $
        PutSlottingData genesisSlottingData :
        PutEpochProposers mempty :
        SetAdopted genesisBlockVersion genesisBvd :
        map ConfirmVersion genesisSoftwareVersions
  where
    genesisBvd = configBlockVersionData genesisConfig
    genesisSlotDuration = bvdSlotDuration genesisBvd

    genesisEpochDuration :: Microsecond
    genesisEpochDuration = fromIntegral (configEpochSlots genesisConfig) * convertUnit genesisSlotDuration

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

-- proposals added by PutProposal, and removed by DeleteProposal
-- upModifierToBatch takes a list of proposals to add&delete
-- listed via getAllProposals, getOldProposals, getDeepProposals, getProposalsByApp
-- does not contain confirmed proposals
instance DBIteratorClass PropIter where
    type IterKey PropIter = UpId
    type IterValue PropIter = ProposalState
    iterKeyPrefix = iterationPrefix

proposalSource ::
       (MonadDBRead m)
    => ConduitT () (IterType PropIter) (ResourceT m) ()
proposalSource = dbIterSource GStateDB (Proxy @PropIter)

getAllProposals :: (MonadDBRead m, MonadUnliftIO m) => m [(UpId, ProposalState)]
getAllProposals = do
  runConduitRes $ proposalSource .| CL.consume

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
    :: (MonadDBRead m, MonadUnliftIO m)
    => UpdateConfiguration
    -> Maybe NumSoftwareVersion
    -> m [ConfirmedProposalState]
getConfirmedProposals uc reqNsv =
    runConduitRes $
    dbIterSource GStateDB (Proxy @ConfPropIter) .| CL.mapMaybe onItem .|
    CL.consume
  where
    onItem (SoftwareVersion {..}, cps)
        | Nothing <- reqNsv = Just cps
        | Just v <- reqNsv
        , hasOurSystemTag cps && svAppName == ourAppName uc && svNumber > v =
            Just cps
        | otherwise = Nothing
    hasOurSystemTag ConfirmedProposalState {..} =
        isJust $ upData cpsUpdateProposal ^. at (ourSystemTag uc)

----------------------------------------------------------------------------
-- Keys ('us' prefix stands for Update System)
----------------------------------------------------------------------------

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


