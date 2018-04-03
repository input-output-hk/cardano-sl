{-# LANGUAGE TypeFamilies #-}

-- | Instance of MoandPollRead which uses DB.

module Pos.Update.Poll.DBPoll
       ( DBPoll
       , runDBPoll
       ) where

import           Universum hiding (id)

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import qualified Ether
import           System.Wlog (WithLogger)
import           UnliftIO (MonadUnliftIO)

import           Pos.Core (Coin, HasGenesisBlockVersionData)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Lrc.Context (HasLrcContext, lrcActionOnEpochReason)
import           Pos.Lrc.DB.Issuers (getIssuersStakes)
import           Pos.Lrc.Types (FullRichmenData)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import qualified Pos.Update.DB as GS
import           Pos.Update.Lrc (tryGetUSRichmen)
import           Pos.Update.Poll.Class (MonadPollRead (..))

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
         , WithLogger m
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
