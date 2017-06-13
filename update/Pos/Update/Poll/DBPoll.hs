{-# LANGUAGE TypeFamilies #-}

-- | Instance of MoandPollRead which uses DB.

module Pos.Update.Poll.DBPoll
       ( DBPoll
       , runDBPoll
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Data.HashMap.Strict          as HM
import qualified Ether
import           System.Wlog                  (WithLogger)

import           Pos.Core                     (Coin)
import           Pos.DB.Class                 (MonadDBRead, MonadRealDB)
import           Pos.Lrc.Context              (LrcContext, lrcActionOnEpochReason)
import           Pos.Lrc.DB.Issuers           (getIssuersStakes)
import           Pos.Lrc.Types                (FullRichmenData)
import qualified Pos.Update.DB                as GS
import           Pos.Update.Poll.Class        (MonadPollRead (..))
import           Pos.Update.RichmenComponent  (getRichmenUS)

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

data DBPollTag

type DBPoll = Ether.TaggedTrans DBPollTag IdentityT

runDBPoll :: DBPoll m a -> m a
runDBPoll = coerce

instance (MonadDBRead m, MonadRealDB m, WithLogger m, Ether.MonadReader' LrcContext m) =>
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
    getEpochTotalStake e = fmap fst <$> getRichmenUS e
    getRichmanStake e id = (findStake =<<) <$> getRichmenUS e
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
