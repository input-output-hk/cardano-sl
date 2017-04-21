-- | Pure Poll

module Pos.Update.Poll.Pure
       ( PurePoll (..)
       , evalPurePollWithLogger
       , execPurePollWithLogger
       , runPurePollWithLogger
       ) where

import           Universum

import           Control.Lens               (at, to, (%=), (.=))
import qualified Data.HashMap.Strict        as HM
import           System.Wlog                (CanLog, HasLoggerName (..), LogEvent,
                                             NamedPureLogger, logWarning, runNamedPureLog)

import           Pos.Crypto                 (hash)
import           Pos.Types                  (SoftwareVersion (..))
import           Pos.Update.Poll.Class      (MonadPoll (..), MonadPollRead (..))
import qualified Pos.Update.Poll.PollState  as Poll
import           Pos.Update.Poll.Types      (BlockVersionState (..),
                                             DecidedProposalState (..),
                                             UndecidedProposalState (..),
                                             cpsSoftwareVersion, propStateToEither)

newtype PurePoll a = PurePoll
    { getPurePoll :: NamedPureLogger (State Poll.PollState) a
    } deriving (Functor, Applicative, Monad, CanLog, HasLoggerName)

instance HasLoggerName Identity where
    getLoggerName = mempty

    modifyLoggerName = flip const

runPurePollWithLogger :: Poll.PollState -> PurePoll a -> (a, Poll.PollState, [LogEvent])
runPurePollWithLogger ps =
    (\((a, c), b) -> (a, b, c)) . flip runState ps . runNamedPureLog . getPurePoll

evalPurePollWithLogger :: Poll.PollState -> PurePoll a -> a
evalPurePollWithLogger r = view _1 . runPurePollWithLogger r

execPurePollWithLogger :: Poll.PollState -> PurePoll a -> Poll.PollState
execPurePollWithLogger r = view _2 . runPurePollWithLogger r

instance MonadPollRead PurePoll where
    getBVState bv = PurePoll $ use $ Poll.psBlockVersions . at bv
    getProposedBVs = PurePoll $ use $ Poll.psBlockVersions . to HM.keys
    getEpochProposers = PurePoll $ use $ Poll.psEpochProposers
    getConfirmedBVStates = PurePoll $ use $ Poll.psBlockVersions . to HM.toList
    getAdoptedBVFull = PurePoll $ use $ Poll.psAdoptedBV
    getLastConfirmedSV an = PurePoll $ use $ Poll.psConfirmedBVs . at an
    getProposal ui = PurePoll $ use $ Poll.psActiveProposals . at ui
    getProposalsByApp an = PurePoll $ do
        delActiveProposalsIndices <- use Poll.psActivePropsIdx
        activeProposals           <- use Poll.psActiveProposals
        propGetByApp delActiveProposalsIndices activeProposals
      where
        propGetByApp appHashmap upIdHashmap =
            case HM.lookup an appHashmap of
                Nothing -> do
                    logWarning $
                        "getProposalsByApp: unknown application name " <> pretty an
                    pure []
                Just hashset -> do
                    let uidList = toList hashset
                        propStateList = map (\u -> (u, HM.lookup u upIdHashmap)) uidList
                    -- 'sequence :: [Maybe ProposalState] -> Maybe [ProposalState]'
                    fromMaybe [] . sequence . map snd <$> filterM filterFun propStateList
        filterFun (uid, Nothing) = do
            logWarning $ "getProposalsByApp: unknown update id " <> pretty uid
            pure False
        filterFun (_, Just _) = pure True
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
        PurePoll $ use $ Poll.psIssuersStakes . to (HM.lookup si <=< HM.lookup ei)
    getSlottingData = PurePoll $ use Poll.psSlottingData

instance MonadPoll PurePoll where
    putBVState bv bvs = PurePoll $ Poll.psBlockVersions . at bv .= Just bvs
    delBVState bv = PurePoll $ Poll.psBlockVersions . at bv .= Nothing
    setAdoptedBV bv = do
        bvs <- getBVState bv
        case bvs of
            Nothing ->
                logWarning $
                    "setAdoptedBV: unknown version " <> pretty bv -- can't happen actually
            Just (bvsData -> bvd) -> PurePoll $ Poll.psAdoptedBV .= (bv, bvd)
    setLastConfirmedSV SoftwareVersion {..} =
        PurePoll $ Poll.psConfirmedBVs . at svAppName .= Just svNumber
    delConfirmedSV an = PurePoll $ Poll.psConfirmedBVs . at an .= Nothing
    addConfirmedProposal cps =
        PurePoll $ Poll.psConfirmedProposals . at (cpsSoftwareVersion cps) .= Just cps
    delConfirmedProposal sv = PurePoll $ Poll.psConfirmedProposals . at sv .= Nothing
    insertActiveProposal p = PurePoll $ Poll.psActiveProposals %= decideProp p
      where
          decideProp ps = HM.insert (getUpId ps) ps
          getUpId = either hashUProp (hashUProp . dpsUndecided) . propStateToEither
          hashUProp = hash . upsProposal
    deactivateProposal ui = PurePoll $ Poll.psActiveProposals . at ui .= Nothing
    setSlottingData sd = PurePoll $ Poll.psSlottingData .= sd
    setEpochProposers hs = PurePoll $ Poll.psEpochProposers .= hs
