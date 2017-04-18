-- | Pure Poll

module Pos.Update.Poll.Pure
       ( PurePoll (..)
       ) where

import           Universum

import           Control.Lens               (to, (%=), (.=))
import qualified Data.HashMap.Strict        as HM
import           System.Wlog                (CanLog, HasLoggerName, NamedPureLogger)

import           Pos.Crypto                 (hash)
import           Pos.Update.Poll.Class      (MonadPoll (..), MonadPollRead (..))
import qualified Pos.Update.Poll.PollState  as Poll
import           Pos.Update.Poll.Types      (DecidedProposalState (..),
                                             UndecidedProposalState (..),
                                             propStateToEither)

newtype PurePoll a = PurePoll
    { getPurePoll :: NamedPureLogger (State Poll.PollState) a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName)

instance MonadPollRead PurePoll where
    getBVState bv = PurePoll $ use $ Poll.psBlockVersions . to (HM.lookup bv)
    getProposedBVs = PurePoll $ use $ Poll.psBlockVersions . to HM.keys
    getEpochProposers = PurePoll $ use $ Poll.psEpochProposers
    getConfirmedBVStates = PurePoll $ use $ Poll.psBlockVersions . to HM.toList
    getAdoptedBVFull = PurePoll $ use $ Poll.psAdoptedBV
    getLastConfirmedSV an = PurePoll $ use $ Poll.psConfirmedBVs . to (HM.lookup an)
    getProposal ui = PurePoll $ use $ Poll.psActiveProposals . to (HM.lookup ui)
    getProposalsByApp an = PurePoll $ do
        delActiveProposalsIndices <- use Poll.psDelActivePropsIdx
        activeProposals           <- use Poll.psActiveProposals
        pure $ propGetByApp delActiveProposalsIndices activeProposals
      where propGetByApp appHashmap upIdHashmap = fromMaybe [] $
                do hashset <- HM.lookup an appHashmap
                   let uidList = toList hashset
                       propStateList = fmap (flip HM.lookup upIdHashmap) uidList
                   sequence propStateList
    getConfirmedProposals = PurePoll $ use $ Poll.psConfirmedProposals . to HM.elems
    getEpochTotalStake ei =
        PurePoll $ use $ Poll.psFullRichmenData . to ((Just . fst) <=< HM.lookup ei)
    getRichmanStake ei si =
        PurePoll $ use $ Poll.psFullRichmenData .
            to ((HM.lookup si . snd) <=< HM.lookup ei)
    getOldProposals si =
        PurePoll $ use $ Poll.psActiveProposals . to (filter ((<= si) . upsSlot) .
                                                      lefts .
                                                      map propStateToEither .
                                                      HM.elems)
    getDeepProposals cd =
        PurePoll $ use $ Poll.psActiveProposals .
            to (filter ((> (Just cd)) . dpsDifficulty) .
                rights .
                map propStateToEither .
                HM.elems)
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
        addProp <$> use Poll.psSoftwareVersion <*> use Poll.psConfirmedProposals
      where
        addProp sv svMap = HM.insert sv cps svMap
    delConfirmedProposal sv = PurePoll $ Poll.psConfirmedProposals %= HM.delete sv
    insertActiveProposal p = PurePoll $ Poll.psActiveProposals %= decideProp p
      where
          decideProp ps = HM.insert (getUpId ps) ps
          getUpId = either hashUProp (hashUProp . dpsUndecided) . propStateToEither
          hashUProp = hash . upsProposal
    deactivateProposal ui = PurePoll $ Poll.psActiveProposals %= HM.delete ui
    setSlottingData sd = PurePoll $ Poll.psSlottingData .= sd
    setEpochProposers hs = PurePoll $ Poll.psEpochProposers .= hs
