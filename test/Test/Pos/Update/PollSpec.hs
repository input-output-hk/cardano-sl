-- | Specification for submodules of Pos.Update.Poll

module Test.Pos.Update.PollSpec
       ( spec
       ) where

import           Universum

import           Data.DeriveTH         (derive, makeArbitrary)
-- import qualified Data.HashSet          as HS
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
-- import           Test.QuickCheck       (Arbitrary (..), Property, choose, conjoin, (===))
import           Test.QuickCheck       (Arbitrary (..), Property, choose, (===))

-- import           Pos.Crypto            (hash)
import           Pos.Slotting.Types    (SlottingData)
import           Pos.Types             (ApplicationName, BlockVersion,
                                        SoftwareVersion (..), StakeholderId)
-- import           Pos.Types             (ApplicationName, BlockVersion,
--                                         SoftwareVersion (..), StakeholderId, addressHash)
import qualified Pos.Update            as Poll
-- import qualified Pos.Util.Modifier     as MM

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
-- FIXME: enable the test below!
--        describe "PurePoll" $ smaller 30 $ do
--            prop
--                "applying a series of modifications to a modifier and then applying it to\
--                \ a poll state is the same as applying the modifications directly to the\
--                \ poll state"
--                applyActions

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
    | SetSlottingData SlottingData
    | SetEpochProposers (HashSet StakeholderId)
    deriving (Show, Eq)

-- actionToMonad :: Poll.MonadPoll m => PollAction -> m ()
-- actionToMonad (PutBVState bv bvs)        = Poll.putBVState bv bvs
-- actionToMonad (DelBVState bv)            = Poll.delBVState bv
-- actionToMonad (SetAdoptedBV bv)          = Poll.setAdoptedBV bv
-- actionToMonad (SetLastConfirmedSV sv)    = Poll.setLastConfirmedSV sv
-- actionToMonad (DelConfirmedSV an)        = Poll.delConfirmedSV an
-- actionToMonad (AddConfirmedProposal cps) = Poll.addConfirmedProposal cps
-- actionToMonad (DelConfirmedProposal sv)  = Poll.delConfirmedProposal sv
-- actionToMonad (InsertActiveProposal ps)  = Poll.insertActiveProposal ps
-- actionToMonad (DeactivateProposal ui)    = Poll.deactivateProposal ui
-- actionToMonad (SetSlottingData sd)       = Poll.setSlottingData sd
-- actionToMonad (SetEpochProposers hs)     = Poll.setEpochProposers hs

-- applyActionToModifier :: PollAction -> Poll.PollModifier -> Poll.PollModifier
-- applyActionToModifier (PutBVState bv bvs) = Poll.pmBVsL %~ MM.insert bv bvs
-- applyActionToModifier (DelBVState bv) = Poll.pmBVsL %~ MM.delete bv
-- applyActionToModifier (SetAdoptedBV bv) = \p ->
--     case MM.lookup (const Nothing) bv (Poll.pmBVs p) of
--         Nothing                    -> p
--         Just (Poll.bvsData -> bvd) -> p { Poll.pmAdoptedBVFull = Just (bv, bvd) }
-- applyActionToModifier (SetLastConfirmedSV SoftwareVersion {..}) =
--     Poll.pmConfirmedL %~ MM.insert svAppName svNumber
-- applyActionToModifier (DelConfirmedSV an) = Poll.pmConfirmedL %~ MM.delete an
-- applyActionToModifier (AddConfirmedProposal cps) =
--     Poll.pmConfirmedPropsL %~ MM.insert (Poll.cpsSoftwareVersion cps) cps
-- applyActionToModifier (DelConfirmedProposal sv) = Poll.pmConfirmedPropsL %~ MM.delete sv
-- applyActionToModifier (InsertActiveProposal ps) = \p ->
--     let up@Poll.UnsafeUpdateProposal{..} = Poll.psProposal ps
--         upId = hash up
--         p' = case MM.lookup (const Nothing) upId (Poll.pmActiveProps p) of
--             Nothing -> p
--             Just _ -> p & Poll.pmEpochProposersL %~ fmap (HS.insert (addressHash upFrom))
--     in p' & (Poll.pmActivePropsL %~ MM.insert upId ps)
-- applyActionToModifier (DeactivateProposal ui) = \p ->
--     let proposal = MM.lookup (const Nothing) ui (Poll.pmActiveProps p)
--     in case proposal of
--            Nothing -> p
--            Just ps ->
--                let up = Poll.psProposal ps
--                    upId = hash up
--                in p & (Poll.pmActivePropsL %~ MM.delete upId)
--
-- applyActionToModifier (SetSlottingData sd) = Poll.pmSlottingDataL .~ (Just sd)
-- applyActionToModifier (SetEpochProposers hs) = Poll.pmEpochProposersL .~ (Just hs)

-- type PollActions = [PollAction]

-- applyActions
--     :: Poll.PollState -> PollActions -> Property
-- applyActions ps actionList =
--     let pollSts = fmap (actionToMonad @Poll.PurePoll) actionList
--         -- 'resultModifiers' has an additional 'mempty' poll modifier up front, so we
--         -- add the initial poll state at the head of 'resultPStates' to make up for that.
--         resultModifiers = scanl (flip applyActionToModifier) mempty actionList
--         resultPStates = ps : scanl Poll.execPurePollWithLogger ps pollSts
--         newPollStates = scanl (flip Poll.modifyPollState) ps resultModifiers
--     in conjoin $ zipWith (===) resultPStates newPollStates

derive makeArbitrary ''PollAction
