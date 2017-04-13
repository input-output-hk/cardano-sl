-- | Pure Poll

module Pos.Update.Poll.Pure
       ( PurePoll (..)
       ) where

import           Control.Lens               (to, (%=), (.=))
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           System.Wlog                (CanLog, HasLoggerName, NamedPureLogger)
import           Universum                  hiding (toList)

import           Pos.Update.Poll.Class      (MonadPoll (..), MonadPollRead (..))
import qualified Pos.Update.Poll.PollState  as Poll
import           Pos.Update.Poll.Types      (DecidedProposalState (dpsDifficulty),
                                             ProposalState (..),
                                             UndecidedProposalState (upsSlot))

newtype PurePoll a = PurePoll
    { getPurePoll :: NamedPureLogger (State Poll.PollState) a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName)

instance MonadPollRead PurePoll where
    getBVState bv = PurePoll $ use $ Poll.psBlockVersions . to (HM.lookup bv)
    getProposedBVs = PurePoll $ use $ Poll.psBlockVersions . to (HM.keys)
    getEpochProposers = PurePoll $ use $ Poll.psEpochProposers
    getConfirmedBVStates = PurePoll $ use $ Poll.psBlockVersions . to (HM.toList)
    getAdoptedBVFull = PurePoll $ use $ Poll.psAdoptedBV
    getLastConfirmedSV an = PurePoll $ use $ Poll.psConfirmedBVs . to (HM.lookup an)
    getProposal ui = PurePoll $ use $ Poll.psActiveProposals . to (HM.lookup ui)
    getProposalsByApp an = PurePoll $ do
        let propGetByApp appHashmap upIdHashmap = fromMaybe [] $
                do hashset <- HM.lookup an appHashmap
                   let uidList = HS.toList hashset
                       propStateList =
                           fmap (flip HM.lookup upIdHashmap) uidList
                   sequence propStateList
        propGetByApp <$> use Poll.psDelActivePropsIdx <*> use Poll.psActiveProposals
    getConfirmedProposals = PurePoll $ use $ Poll.psConfirmedProposals . to (HM.elems)
    getEpochTotalStake ei = PurePoll $ use $ Poll.psStakePerEpoch . to (HM.lookup ei)
    getRichmanStake ei si =
        PurePoll $ use $ Poll.psMultiRichmenStake . to (HM.lookup ei >=> HM.lookup si)
    getOldProposals si =
        PurePoll $ use $ Poll.psUndecidedProposals . to (filter ((<= si) . upsSlot))
    getDeepProposals cd =
        PurePoll $ use $ Poll.psDecidedProposals .
            to (filter ((> (Just cd)) . dpsDifficulty))
    getBlockIssuerStake ei si =
        PurePoll $ use $ Poll.psIssuersStakes . to (HM.lookup ei >=> HM.lookup si)
    getSlottingData = PurePoll $ use Poll.psSlottingData

instance MonadPoll PurePoll where
    putBVState bv bvs = PurePoll $ Poll.psBlockVersions %= HM.insert bv bvs
    delBVState bv = PurePoll $ Poll.psBlockVersions %= HM.delete bv
    setAdoptedBV bv = PurePoll $ Poll.psAdoptedBV %= (_1 .~ bv)
    setLastConfirmedSV sv = PurePoll $ Poll.psSoftwareVersion .= sv
    delConfirmedSV an = PurePoll $ Poll.psConfirmedBVs %= HM.delete an
    addConfirmedProposal cps = PurePoll $ void $ do
        let addProp sv svMap = HM.insert sv cps svMap
        addProp <$> use Poll.psSoftwareVersion <*> use Poll.psConfirmedProposals
    delConfirmedProposal sv = PurePoll $ Poll.psConfirmedProposals %= HM.delete sv
    insertActiveProposal p = PurePoll $ void $ do
        let decideProp =
                case p of
                    PSDecided ps -> (ps :)
                    _ -> identity
        decideProp <$> use Poll.psDecidedProposals
    deactivateProposal ui = PurePoll $ Poll.psActiveProposals %= HM.delete ui
    setSlottingData sd = PurePoll $ Poll.psSlottingData .= sd
    setEpochProposers hs = PurePoll $ Poll.psEpochProposers .= hs
