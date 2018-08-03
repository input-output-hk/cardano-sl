-- | Pure Poll

module Pos.DB.Update.Poll.Pure
       ( PurePoll (..)
       , execPurePollWithLogger
       ) where

import           Universum

import           Control.Lens (at, mapped, to, uses, (%=), (.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import           Pos.Chain.Update (BlockVersionState (..),
                     DecidedProposalState (..), MonadPoll (..),
                     MonadPollRead (..), UndecidedProposalState (..), applyBVM,
                     cpsSoftwareVersion, propStateToEither, psProposal)
import           Pos.Core.Update (SoftwareVersion (..), UpdateProposal (..))
import           Pos.Crypto (hash)
import qualified Pos.DB.Update.Poll.PollState as Poll
--import           Pos.Util.Trace.Named (TraceNamed, logDebug, logWarning,
--                     natTrace)

newtype PurePoll a = PurePoll
    { getPurePoll :: StateT Poll.PollState Identity a
    } deriving (Functor, Applicative, Monad, MonadState Poll.PollState)

runPurePollWithLogger :: Poll.PollState -> PurePoll a -> (a, Poll.PollState)
runPurePollWithLogger ps pp =
    let innerMonad = usingStateT ps . getPurePoll $ pp
    in  (\((a, finalState)) -> (a, finalState)) . runIdentity $ innerMonad

execPurePollWithLogger :: Poll.PollState -> PurePoll a -> Poll.PollState
execPurePollWithLogger r = view _2 . runPurePollWithLogger r

instance MonadPollRead PurePoll where
    getBVState bv = PurePoll $ use $ Poll.psBlockVersions . at bv
    getProposedBVs = PurePoll $ use $ Poll.psBlockVersions . to HM.keys
    getEpochProposers = PurePoll $ use $ Poll.psEpochProposers
    getCompetingBVStates = PurePoll $ use $ Poll.psBlockVersions . to HM.toList
    getAdoptedBVFull = PurePoll $ use $ Poll.psAdoptedBV
    getLastConfirmedSV an = PurePoll $ use $ Poll.psConfirmedANs . at an
    getProposal ui = PurePoll $ use $ Poll.psActiveProposals . at ui
    getProposalsByApp _ an = PurePoll $ do
        activeProposalsIndices <- use Poll.psActivePropsIdx
        activeProposals        <- use Poll.psActiveProposals
        propGetByApp activeProposalsIndices activeProposals
      where
        propGetByApp appHashmap upIdHashmap =
            case HM.lookup an appHashmap of
                Nothing -> do
                    --logDebug logTrace $    -- TODO
                    --    "getProposalsByApp: unknown application name " <> pretty an
                    pure []
                Just hashset -> do
                    let uidList = toList hashset
                        propStateList = map (\u -> (u, HM.lookup u upIdHashmap)) uidList
                    fromMaybe [] . traverse snd <$> filterM filterFun propStateList
        filterFun ({-uid-}_, Nothing) = do
            --logWarning logTrace $ "getProposalsByApp: unknown update id " <> pretty uid  -- TODO
            pure False
        filterFun (_, Just _) = pure True
    getConfirmedProposals = PurePoll $ use $ Poll.psConfirmedProposals . to HM.elems
    getEpochTotalStake ei =
        PurePoll $ uses Poll.psFullRichmenData $ (Just . fst) <=< HM.lookup ei
    getRichmanStake ei si =
        PurePoll $ uses Poll.psFullRichmenData $ (HM.lookup si . snd) <=< HM.lookup ei
    getOldProposals si =
        PurePoll $ uses Poll.psActiveProposals $ filter ((<= si) . upsSlot) .
                                                 lefts .
                                                 map propStateToEither .
                                                 HM.elems
    getDeepProposals cd =
        PurePoll $ uses Poll.psActiveProposals $
            filter (maybe False (<= cd) . dpsDifficulty) .
            rights .
            map propStateToEither .
            HM.elems
    getBlockIssuerStake ei si =
        PurePoll $ uses Poll.psIssuersStakes $ HM.lookup si <=< HM.lookup ei
    getSlottingData = PurePoll $ use Poll.psSlottingData

instance MonadPoll PurePoll where
    putBVState bv bvs = PurePoll $ Poll.psBlockVersions . at bv .= Just bvs
    delBVState bv = PurePoll $ Poll.psBlockVersions . at bv .= Nothing
    setAdoptedBV _ bv = do
        bvs <- getBVState bv
        case bvs of
            Nothing ->
                --logWarning logTrace $    -- TODO
                --    "setAdoptedBV: unknown version " <> pretty bv -- can't happen actually
                return ()
            Just (bvsModifier -> bvm) -> PurePoll $ do
                Poll.psAdoptedBV . _1 .= bv
                Poll.psAdoptedBV . _2 %= applyBVM bvm
    setLastConfirmedSV SoftwareVersion {..} =
        PurePoll $ Poll.psConfirmedANs . at svAppName .= Just svNumber
    delConfirmedSV an = PurePoll $ Poll.psConfirmedANs . at an .= Nothing
    addConfirmedProposal cps =
        PurePoll $ Poll.psConfirmedProposals . at (cpsSoftwareVersion cps) .= Just cps
    delConfirmedProposal sv = PurePoll $ Poll.psConfirmedProposals . at sv .= Nothing
    insertActiveProposal p =
        PurePoll $ do
            Poll.psActiveProposals %= decideProp
            Poll.psActivePropsIdx %= addUIdtoApp
      where
          decideProp = HM.insert uId p
          addUIdtoApp = HM.insertWith HS.union appName (HS.singleton uId)

          uProp = psProposal p
          uId = hash uProp
          appName = svAppName . upSoftwareVersion $ uProp
    deactivateProposal ui = PurePoll $ do
         Poll.psActiveProposals . at ui .= Nothing
         Poll.psActivePropsIdx . mapped . at ui .= Nothing
         Poll.psActivePropsIdx %= HM.filter (not . null)
    setSlottingData sd = PurePoll $ Poll.psSlottingData .= sd
    setEpochProposers hs = PurePoll $ Poll.psEpochProposers .= hs
