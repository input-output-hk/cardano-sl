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
import           Pos.Chain.Update (UpdateConfiguration, MonadPollRead (..))
import           Pos.Core (Coin)
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.Lrc (HasLrcContext, getIssuersStakes,
                     lrcActionOnEpochReason, tryGetUSRichmen)
import qualified Pos.DB.Update.GState as GS
import           Pos.Util.Wlog (WithLogger)

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

data DBPollTag

type DBPoll = Ether.ReaderT DBPollTag UpdateConfiguration

runDBPoll :: UpdateConfiguration -> DBPoll m a -> m a
runDBPoll uc action = Ether.runReaderT action uc

instance ( MonadIO m
         , MonadDBRead m
         , MonadUnliftIO m
         , WithLogger m
         , MonadReader ctx m
         , HasLrcContext ctx
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
    getConfirmedProposals = do
        uc <- Ether.ask @DBPollTag
        GS.getConfirmedProposals uc Nothing
    getEpochTotalStake genesisBvd e = fmap fst <$> tryGetUSRichmen genesisBvd e
    getRichmanStake genesisBvd e id =
        (findStake =<<) <$> tryGetUSRichmen genesisBvd e
      where
        findStake :: FullRichmenData -> Maybe Coin
        findStake = HM.lookup id . snd
    getOldProposals = GS.getOldProposals
    getDeepProposals = GS.getDeepProposals
    getBlockIssuerStake epoch id =
        lift $ lrcActionOnEpochReason epoch
            "couldn't get issuers's stakes"
            (fmap (Just . HM.lookup id) . getIssuersStakes)
    getSlottingData = GS.getSlottingData
