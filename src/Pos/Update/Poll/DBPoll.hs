{-# LANGUAGE TypeFamilies #-}

-- | Instance of MoandPollRead which uses DB.

module Pos.Update.Poll.DBPoll
       ( DBPoll
       , runDBPoll
       ) where

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Data.HashMap.Strict          as HM
import qualified Ether
import           Pos.Context                  (lrcActionOnEpochReason)
import           System.Wlog                  (WithLogger)
import           Universum

import           Pos.DB.Class                 (MonadDB, MonadDBPure)
import           Pos.Lrc.Context              (LrcContext)
import           Pos.Lrc.DB                   (getIssuersStakes, getRichmenUS)
import           Pos.Lrc.Types                (FullRichmenData)
import           Pos.Types                    (Coin)
import qualified Pos.Update.DB                as GS
import           Pos.Update.Poll.Class        (MonadPollRead (..))

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

data DBPollTag

type DBPoll = Ether.TaggedTrans DBPollTag IdentityT

runDBPoll :: DBPoll m a -> m a
runDBPoll = coerce

instance (MonadDBPure m, MonadDB m, WithLogger m, Ether.MonadReader' LrcContext m) =>
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
