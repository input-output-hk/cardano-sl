{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Part of GState DB which stores data necessary for update system.

module Pos.Update.DB
       (
         -- * Getters
         getAdoptedBV
       , getAdoptedBVData
       , getAdoptedBVFull
       , getBVState
       , getProposalState
       , getAppProposal
       , getProposalsByApp
       , getConfirmedSV
       , getMaxBlockSize
       , getSlottingData
       , getEpochProposers

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

       , BVIter
       , getProposedBVs
       , getConfirmedBVStates
       , getProposedBVStates
       ) where

import           Data.Time.Units            (convertUnit)
import qualified Database.RocksDB           as Rocks
import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Binary.Class           (encodeStrict)
import           Pos.Binary.Infra.Slotting  ()
import           Pos.Binary.Update          ()
import           Pos.Core                   (ApplicationName, BlockVersion,
                                             ChainDifficulty, NumSoftwareVersion, SlotId,
                                             SoftwareVersion (..), StakeholderId,
                                             Timestamp (..))
import           Pos.Core.Constants         (epochSlots)
import           Pos.Crypto                 (hash)
import           Pos.DB.Class               (MonadDB, getUtxoDB)
import           Pos.DB.Error               (DBError (DBMalformed))
import           Pos.DB.Functions           (RocksBatchOp (..), encodeWithKeyPrefix,
                                             rocksWriteBatch)
import           Pos.DB.GState.Common       (gsGetBi)
import           Pos.DB.Holder              (DBHolder)
import           Pos.DB.Iterator            (DBIteratorClass (..), DBnIterator,
                                             DBnMapIterator, IterType, runDBnIterator,
                                             runDBnMapIterator)
import qualified Pos.DB.Limits              as DBLimits
import           Pos.DB.Types               (NodeDBs (..))
import           Pos.Slotting.Types         (EpochSlottingData (..), SlottingData (..))
import           Pos.Update.Constants       (genesisBlockVersion, genesisBlockVersionData,
                                             genesisSlotDuration, genesisSoftwareVersions,
                                             ourAppName)
import           Pos.Update.Core            (BlockVersionData (..), UpId,
                                             UpdateProposal (..))
import           Pos.Update.Poll.Types      (BlockVersionState (..),
                                             ConfirmedProposalState,
                                             DecidedProposalState (dpsDifficulty),
                                             ProposalState (..),
                                             UndecidedProposalState (upsSlot),
                                             cpsSoftwareVersion, psProposal)
import           Pos.Util.Iterator          (MonadIterator (..))
import           Pos.Util.Util              (maybeThrow)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get last adopted block version.
getAdoptedBV :: MonadDB m => m BlockVersion
getAdoptedBV = fst <$> getAdoptedBVFull

-- | Get state of last adopted BlockVersion.
getAdoptedBVData :: MonadDB m => m BlockVersionData
getAdoptedBVData = snd <$> getAdoptedBVFull

-- | Get last adopted BlockVersion and data associated with it.
getAdoptedBVFull :: MonadDB m => m (BlockVersion, BlockVersionData)
getAdoptedBVFull = maybeThrow (DBMalformed msg) =<< getAdoptedBVFullMaybe
  where
    msg =
        "Update System part of GState DB is not initialized (last adopted BV is missing)"

-- | Get maximum block size (in bytes).
getMaxBlockSize :: MonadDB m => m Byte
getMaxBlockSize = bvdMaxBlockSize <$> getAdoptedBVData

-- | Get 'BlockVersionState' associated with given BlockVersion.
getBVState :: MonadDB m => BlockVersion -> m (Maybe BlockVersionState)
getBVState = gsGetBi . bvStateKey

-- | Get state of UpdateProposal for given UpId
getProposalState :: MonadDB m => UpId -> m (Maybe ProposalState)
getProposalState = gsGetBi . proposalKey

-- | Get UpId of current proposal for given appName
getAppProposal :: MonadDB m => ApplicationName -> m (Maybe UpId)
getAppProposal = gsGetBi . proposalAppKey

-- | Get states of all active 'UpdateProposal's for given 'ApplicationName'.
getProposalsByApp :: MonadDB m => ApplicationName -> m [ProposalState]
getProposalsByApp appName = runProposalMapIterator (step []) snd
  where
    step res = nextItem >>= maybe (pure res) (onItem res)
    onItem res e
        | appName == (svAppName $ upSoftwareVersion $ psProposal e) = step (e:res)
        | otherwise = step res

-- | Get last confirmed SoftwareVersion of given application.
getConfirmedSV :: MonadDB m => ApplicationName -> m (Maybe NumSoftwareVersion)
getConfirmedSV = gsGetBi . confirmedVersionKey

-- | Get most recent 'SlottingData'.
getSlottingData :: MonadDB m => m SlottingData
getSlottingData = maybeThrow (DBMalformed msg) =<< gsGetBi slottingDataKey
  where
    msg =
        "Update System part of GState DB is not initialized (slotting data is missing)"

-- | Get proposers for current epoch.
getEpochProposers :: MonadDB m => m (HashSet StakeholderId)
getEpochProposers = maybeThrow (DBMalformed msg) =<< gsGetBi epochProposersKey
  where
    msg =
        "Update System part of GState DB is not initialized (epoch proposers are missing)"
----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

data UpdateOp
    = PutProposal !ProposalState
    | DeleteProposal !UpId !ApplicationName
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

prepareGStateUS :: MonadDB m => Timestamp -> m ()
prepareGStateUS systemStart =
    unlessM isInitialized $ do
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
        db <- getUtxoDB
        flip rocksWriteBatch db $
            PutSlottingData genesisSlottingData :
            PutEpochProposers mempty :
            SetAdopted genesisBlockVersion genesisBlockVersionData :
            map ConfirmVersion genesisSoftwareVersions

isInitialized :: MonadDB m => m Bool
isInitialized = isJust <$> getAdoptedBVFullMaybe

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data PropIter

instance DBIteratorClass PropIter where
    type IterKey PropIter = UpId
    type IterValue PropIter = ProposalState
    iterKeyPrefix _ = iterationPrefix

runProposalIterator
    :: forall m a . MonadDB m
    => DBnIterator PropIter a -> m a
runProposalIterator = runDBnIterator @PropIter _gStateDB

runProposalMapIterator
    :: forall v m a . MonadDB m
    => DBnMapIterator PropIter v a -> (IterType PropIter -> v) -> m a
runProposalMapIterator = runDBnMapIterator @PropIter _gStateDB

-- TODO: it can be optimized by storing some index sorted by
-- 'SlotId's, but I don't think it may be crucial.
-- | Get all proposals which were issued no later than given slot.
getOldProposals
    :: MonadDB m
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
    :: MonadDB m
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
    type IterKey ConfPropIter = SoftwareVersion
    type IterValue ConfPropIter = ConfirmedProposalState
    iterKeyPrefix _ = confirmedIterationPrefix

-- | Get confirmed proposals which update our application and have
-- version bigger than argument (or all proposals if 'Nothing' is
-- passed). For instance, current software version can be passed to
-- this function to get all proposals with bigger version.
getConfirmedProposals
    :: MonadDB m
    => Maybe NumSoftwareVersion -> m [ConfirmedProposalState]
getConfirmedProposals reqNsv = runDBnIterator @ConfPropIter _gStateDB (step [])
  where
    step res = nextItem >>= maybe (pure res) (onItem res)
    onItem res (SoftwareVersion {..}, cps) =
        step $
        case reqNsv of
            Nothing -> cps : res
            Just v
                | svAppName == ourAppName && svNumber > v -> cps : res
                | otherwise -> res

-- Iterator by block versions
data BVIter

instance DBIteratorClass BVIter where
    type IterKey BVIter = BlockVersion
    type IterValue BVIter = BlockVersionState
    iterKeyPrefix _ = bvStateIterationPrefix

-- | Get all proposed 'BlockVersion's.
getProposedBVs :: MonadDB m => m [BlockVersion]
getProposedBVs = runDBnMapIterator @BVIter _gStateDB (step []) fst
  where
    step res = nextItem >>= maybe (pure res) (onItem res)
    onItem res = step . (: res)

getProposedBVStates :: MonadDB m => m [BlockVersionState]
getProposedBVStates = runDBnMapIterator @BVIter _gStateDB (step []) snd
  where
    step res = nextItem >>= maybe (pure res) (onItem res)
    onItem res = step . (: res)

-- | Get all confirmed 'BlockVersion's and their states.
getConfirmedBVStates
    :: MonadDB m
    => m [(BlockVersion, BlockVersionState)]
getConfirmedBVStates = runDBnIterator @BVIter _gStateDB (step [])
  where
    step res = nextItem >>= maybe (pure res) (onItem res)
    onItem res (bv, bvs@BlockVersionState {..})
        | bvsIsConfirmed = step ((bv, bvs) : res)
        | otherwise = step res

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

proposalAppKey :: ApplicationName -> ByteString
proposalAppKey = mappend "us/an/" . encodeStrict

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
    :: MonadDB m
    => m (Maybe (BlockVersion, BlockVersionData))
getAdoptedBVFullMaybe = gsGetBi adoptedBVKey

----------------------------------------------------------------------------
-- Some instance
----------------------------------------------------------------------------

instance (MonadIO m, MonadCatch m) =>
         DBLimits.MonadDBLimits (DBHolder m) where
    getMaxBlockSize = getMaxBlockSize
    getMaxHeaderSize = bvdMaxHeaderSize <$> getAdoptedBVData
    getMaxTxSize = bvdMaxTxSize <$> getAdoptedBVData
    getMaxProposalSize = bvdMaxProposalSize <$> getAdoptedBVData
