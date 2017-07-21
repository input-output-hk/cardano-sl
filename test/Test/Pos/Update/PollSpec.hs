{-# LANGUAGE TemplateHaskell #-}

-- | Specification for submodules of Pos.Update.Poll

module Test.Pos.Update.PollSpec
       ( spec
       ) where

import           Universum

import           Control.Lens          (at)
import           Data.DeriveTH         (derive, makeArbitrary)
import qualified Data.HashSet          as HS
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck       (Arbitrary (..), Property, choose, conjoin, (===))

import           Pos.Core              (EpochIndex)
import           Pos.Crypto            (hash)
import           Pos.Slotting.Types    (EpochSlottingData)
import           Pos.Types             (ApplicationName, BlockVersion,
                                        SoftwareVersion (..), StakeholderId, addressHash)
import qualified Pos.Update            as Poll
import qualified Pos.Util.Modifier     as MM

import           Test.Pos.Util         (formsMonoid)

spec :: Spec
spec = describe "Poll" $ do
    let smaller n = modifyMaxSuccess (const n)
    describe "modifyPollModifier" $ smaller 30 $ do
        prop
            "poll modifiers form a commutative monoid under 'modifyPollModifier'"
            modifyPollFormsMonoid
    describe "PollState" $ smaller 30 $ do
        prop
            "applying two poll modifiers in sequence to the poll state is equivalent\
            \ to combining them and applying the resulting modifier"
            modifyPollStateWithModifiers
        describe "PurePoll" $ smaller 30 $ do
            prop
                "applying a series of modifications to a modifier and then applying it to\
                \ a poll state is the same as applying the modifications directly to the\
                \ poll state"
                applyActions

modifyPollFormsMonoid
    :: Poll.PollModifier
    -> Poll.PollModifier
    -> Poll.PollModifier
    -> Property
modifyPollFormsMonoid = formsMonoid

modifyPollStateWithModifiers
    :: Poll.PollState
    -> Poll.PollModifier
    -> Poll.PollModifier
    -> Property
modifyPollStateWithModifiers pst pm1 pm2 =
    Poll.modifyPollState pm2 (Poll.modifyPollState pm1 pst) ===
    Poll.modifyPollState (pm1 <> pm2) pst

data PollAction
    = PutBVState BlockVersion Poll.BlockVersionState
    | DelBVState BlockVersion
    | SetAdoptedBV BlockVersion
    | SetLastConfirmedSV SoftwareVersion
    | DelConfirmedSV ApplicationName
    | AddConfirmedProposal Poll.ConfirmedProposalState
    | DelConfirmedProposal SoftwareVersion
    | InsertActiveProposal Poll.ProposalState
    | DeactivateProposal Poll.UpId
    | PutEpochSlottingData EpochIndex EpochSlottingData
    | SetEpochProposers (HashSet StakeholderId)
    deriving (Show, Eq)

actionToMonad :: Poll.MonadPoll m => PollAction -> m ()
actionToMonad (PutBVState bv bvs)           = Poll.putBVState bv bvs
actionToMonad (DelBVState bv)               = Poll.delBVState bv
actionToMonad (SetAdoptedBV bv)             = Poll.setAdoptedBV bv
actionToMonad (SetLastConfirmedSV sv)       = Poll.setLastConfirmedSV sv
actionToMonad (DelConfirmedSV an)           = Poll.delConfirmedSV an
actionToMonad (AddConfirmedProposal cps)    = Poll.addConfirmedProposal cps
actionToMonad (DelConfirmedProposal sv)     = Poll.delConfirmedProposal sv
actionToMonad (InsertActiveProposal ps)     = Poll.insertActiveProposal ps
actionToMonad (DeactivateProposal ui)       = Poll.deactivateProposal ui
actionToMonad (PutEpochSlottingData ei esd) = Poll.putEpochSlottingData ei esd
actionToMonad (SetEpochProposers hs)        = Poll.setEpochProposers hs

applyActionToModifier
    :: PollAction
    -> Poll.PollState
    -> Poll.PollModifier
    -> Poll.PollModifier
applyActionToModifier (PutBVState bv bvs) _ = Poll.pmBVsL %~ MM.insert bv bvs
applyActionToModifier (DelBVState bv) _ = Poll.pmBVsL %~ MM.delete bv
applyActionToModifier (SetAdoptedBV bv) pst = \p ->
    case MM.lookup innerLookupFun bv (Poll.pmBVs p) of
        Nothing                    -> p
        Just (Poll.bvsData -> bvd) -> p { Poll.pmAdoptedBVFull = Just (bv, bvd) }
  where
    innerLookupFun k = pst ^. Poll.psBlockVersions . at k
applyActionToModifier (SetLastConfirmedSV SoftwareVersion {..}) _ =
    Poll.pmConfirmedL %~ MM.insert svAppName svNumber
applyActionToModifier (DelConfirmedSV an) _ = Poll.pmConfirmedL %~ MM.delete an
applyActionToModifier (AddConfirmedProposal cps) _ =
    Poll.pmConfirmedPropsL %~ MM.insert (Poll.cpsSoftwareVersion cps) cps
applyActionToModifier (DelConfirmedProposal sv) _ = Poll.pmConfirmedPropsL %~ MM.delete sv
applyActionToModifier (InsertActiveProposal ps) pst = \p ->
    let up@Poll.UnsafeUpdateProposal{..} = Poll.psProposal ps
        upId = hash up
        p' = case MM.lookup innerLookupFun upId (Poll.pmActiveProps p) of
            Nothing -> p
            Just _ -> p & Poll.pmEpochProposersL %~ fmap (HS.insert (addressHash upFrom))
    in p' & (Poll.pmActivePropsL %~ MM.insert upId ps)
  where
    innerLookupFun k = pst ^. Poll.psActiveProposals . at k

applyActionToModifier (DeactivateProposal ui) pst = \p ->
    let proposal = MM.lookup innerLookupFun ui (Poll.pmActiveProps p)
    in case proposal of
           Nothing -> p
           Just ps ->
               let up = Poll.psProposal ps
                   upId = hash up
               in p & (Poll.pmActivePropsL %~ MM.delete upId)
  where
    innerLookupFun k = pst ^. Poll.psActiveProposals . at k

applyActionToModifier (PutEpochSlottingData ei esd) _ = Poll.pmSlottingDataL %~ (MM.insert ei esd)
applyActionToModifier (SetEpochProposers hs) _ = Poll.pmEpochProposersL .~ (Just hs)

type PollActions = [PollAction]

applyActions :: Poll.PollState -> PollActions -> Property
applyActions ps actionList =
    let pollSts = fmap (actionToMonad @Poll.PurePoll) actionList
        -- 'resultModifiers' has a 'mempty' poll modifier up front, so 'newPollStates'
        -- has two 'ps's in the head of the list. As such another 'ps' is added
        -- at the head of 'resultPStates' to make up for that.
        resultModifiers =
            scanl (\pmod act -> applyActionToModifier act ps pmod) mempty actionList
        resultPStates = ps : scanl Poll.execPurePollWithLogger ps pollSts
        newPollStates = scanl (flip Poll.modifyPollState) ps resultModifiers
    in conjoin $ zipWith (===) resultPStates newPollStates

derive makeArbitrary ''PollAction
