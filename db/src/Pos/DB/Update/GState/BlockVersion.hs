{-# LANGUAGE TypeFamilies #-}

-- | Interface for the adopted BV DB

module Pos.DB.Update.GState.BlockVersion
       (
         -- * Getters
         getBVState
       , getAdoptedBV
       , getAdoptedBVFull
       , getAdoptedBVData
       , getConsensusEra

         -- * Iteration
       , BVIter
       , bvSource
       , getProposedBVs
       , getProposedBVStates
       , getCompetingBVStates

         -- * Database keys
       , adoptedBVKey
       , blockIndexKey
       , bvStateKey
       ) where

import           Universum

import           Control.Monad.Trans.Resource (ResourceT)
import           Data.ByteArray (convert)
import           Data.Conduit (ConduitT, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Update (BlockVersion (..), BlockVersionData (..),
                     BlockVersionState, ConsensusEra (..), bvsIsConfirmed,
                     consensusEraBVD)
import           Pos.DB.Class (DBIteratorClass (..), DBTag (..), IterType,
                     MonadDBRead (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (encodeWithKeyPrefix)
import           Pos.DB.GState.Common (gsGetBi)
import           Pos.Util.Util (maybeThrow)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get 'BlockVersionState' associated with given BlockVersion.
getBVState :: MonadDBRead m => BlockVersion -> m (Maybe BlockVersionState)
getBVState = gsGetBi . bvStateKey

-- | Get last adopted block version.
getAdoptedBV :: MonadDBRead m => m BlockVersion
getAdoptedBV = fst <$> getAdoptedBVFull

getAdoptedBVFullMaybe
    :: MonadDBRead m
    => m (Maybe (BlockVersion, BlockVersionData))
getAdoptedBVFullMaybe = gsGetBi adoptedBVKey

-- | Get last adopted BlockVersion and data associated with it.
getAdoptedBVFull :: MonadDBRead m => m (BlockVersion, BlockVersionData)
getAdoptedBVFull = maybeThrow (DBMalformed msg) =<< getAdoptedBVFullMaybe
  where
    msg =
        "Update System part of GState DB is not initialized (last adopted BV is missing)"

-- | Get state of last adopted BlockVersion.
getAdoptedBVData :: MonadDBRead m => m BlockVersionData
getAdoptedBVData = snd <$> getAdoptedBVFull

-- | Get the ConsensusEra from the database.
getConsensusEra :: MonadDBRead m => m ConsensusEra
getConsensusEra = consensusEraBVD <$> getAdoptedBVData

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

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
-- Database keys
----------------------------------------------------------------------------

bvStateIterationPrefix :: ByteString
bvStateIterationPrefix = "us/bvs/"

blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h

bvStateKey :: BlockVersion -> ByteString
bvStateKey = encodeWithKeyPrefix @BVIter

adoptedBVKey :: ByteString
adoptedBVKey = "us/adopted-block-version/"
