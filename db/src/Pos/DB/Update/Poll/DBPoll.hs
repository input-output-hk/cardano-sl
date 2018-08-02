{-# LANGUAGE TypeFamilies #-}

-- | Instance of MoandPollRead which uses DB.

module Pos.DB.Update.Poll.DBPoll
       ( DBPoll
       , runDBPoll
       ) where

import           Universum hiding (id)

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import qualified Ether
import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Lrc (FullRichmenData)
import           Pos.Chain.Update (HasUpdateConfiguration, MonadPollRead (..))
import           Pos.Core (Coin, HasGenesisBlockVersionData)
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.Lrc (HasLrcContext, getIssuersStakes,
                     lrcActionOnEpochReason, tryGetUSRichmen)
import qualified Pos.DB.Update.GState as GS

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

data DBPollTag

type DBPoll = Ether.TaggedTrans DBPollTag IdentityT

runDBPoll :: DBPoll m a -> m a
runDBPoll = coerce

instance ( MonadIO m
         , MonadDBRead m
         , MonadUnliftIO m
         , MonadReader ctx m
         , HasLrcContext ctx
         , HasUpdateConfiguration
         , HasGenesisBlockVersionData
         ) =>
         MonadPollRead (DBPoll m) where
    getBVState = GS.getBVState
    getProposedBVs = GS.getProposedBVs
    getEpochProposers = GS.getEpochProposers
    getCompetingBVStates = GS.getCompetingBVStates
    getAdoptedBVFull = GS.getAdoptedBVFull
    getLastConfirmedSV = GS.getConfirmedSV
    getProposal = GS.getProposalState
    getProposalsByApp = GS.getProposalsByApp
    getConfirmedProposals = GS.getConfirmedProposals Nothing
    getEpochTotalStake e = fmap fst <$> tryGetUSRichmen e
    getRichmanStake e id = (findStake =<<) <$> tryGetUSRichmen e
      where
        findStake :: FullRichmenData -> Maybe Coin
        findStake = HM.lookup id . snd
    getOldProposals = GS.getOldProposals
    getDeepProposals = GS.getDeepProposals
    getBlockIssuerStake epoch id =
        lrcActionOnEpochReason epoch
            "couldn't get issuers's stakes"
            (fmap (Just . HM.lookup id) . getIssuersStakes)
    getSlottingData = GS.getSlottingData
