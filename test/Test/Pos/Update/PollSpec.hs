-- | Specification for submodules of Pos.Update.Poll

module Test.Pos.Update.PollSpec
       ( spec
       ) where

import           Universum

import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (Arbitrary (..), Property, (===), conjoin,
                                            oneof)

import           Pos.Crypto                (hash)
import           Pos.Slotting.Types        (SlottingData)
import           Pos.Types                 (ApplicationName, BlockVersion,
                                            SoftwareVersion (..), StakeholderId,
                                            addressHash)
import qualified Pos.Update                as Poll
import qualified Pos.Util.Modifier         as MM

import           Test.Pos.Util             (formsMonoid)

spec :: Spec
spec = describe "Poll" $ do
    describe "modifyPollModifier" $ do
        prop
            "poll modifiers form a commutative monoid under 'modifyPollModifier'"
            modifyPollFormsMonoid
    describe "PollState" $ do
        prop
            "applying two poll modifiers in sequence to the poll state is equivalent\
            \ to combining them and applying the resulting modifier"
            modifyPollStateWithModifiers
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
    | SetSlottingData SlottingData
    | SetEpochProposers (HashSet StakeholderId)
    deriving (Show, Eq)

actionToMonad :: Poll.MonadPoll m => PollAction -> m ()
actionToMonad (PutBVState bv bvs) = Poll.putBVState bv bvs
actionToMonad (DelBVState bv) = Poll.delBVState bv
actionToMonad (SetAdoptedBV bv) = Poll.setAdoptedBV bv
actionToMonad (SetLastConfirmedSV sv) = Poll.setLastConfirmedSV sv
actionToMonad (DelConfirmedSV an) = Poll.delConfirmedSV an
actionToMonad (AddConfirmedProposal cps) = Poll.addConfirmedProposal cps
actionToMonad (DelConfirmedProposal sv) = Poll.delConfirmedProposal sv
actionToMonad (InsertActiveProposal ps) = Poll.insertActiveProposal ps
actionToMonad (DeactivateProposal ui) = Poll.deactivateProposal ui
actionToMonad (SetSlottingData sd) = Poll.setSlottingData sd
actionToMonad (SetEpochProposers hs) = Poll.setEpochProposers hs

applyActionToModifier :: PollAction -> Poll.PollModifier -> Poll.PollModifier
applyActionToModifier (PutBVState bv bvs) = Poll.pmBVsL %~ MM.insert bv bvs
applyActionToModifier (DelBVState bv) = Poll.pmBVsL %~ MM.delete bv
applyActionToModifier (SetAdoptedBV bv) = \p ->
    case MM.lookup (const Nothing) bv (Poll.pmBVs p) of
        Nothing -> p
        Just (Poll.bvsData -> bvd) -> p { Poll.pmAdoptedBVFull = Just (bv, bvd) }
applyActionToModifier (SetLastConfirmedSV SoftwareVersion {..}) =
    Poll.pmConfirmedL %~ MM.insert svAppName svNumber
applyActionToModifier (DelConfirmedSV an) = Poll.pmConfirmedL %~ MM.delete an
applyActionToModifier (AddConfirmedProposal cps) =
    Poll.pmConfirmedPropsL %~ MM.insert (Poll.cpsSoftwareVersion cps) cps
applyActionToModifier (DelConfirmedProposal sv) = Poll.pmConfirmedPropsL %~ MM.delete sv
applyActionToModifier (InsertActiveProposal ps) = \p ->
    let up@Poll.UnsafeUpdateProposal{upSoftwareVersion = sv, ..} = Poll.psProposal ps
        upId = hash up
        appName = svAppName sv
        alterDel _ Nothing     = Nothing
        alterDel val (Just hs) = Just $ HS.delete val hs
        p' = case MM.lookup (const Nothing) upId (Poll.pmActiveProps p) of
            Nothing -> p
            Just _ -> p & Poll.pmEpochProposersL %~ fmap (HS.insert (addressHash upFrom))
    in p' & ((Poll.pmActivePropsL %~ MM.insert upId ps) .
            (Poll.pmDelActivePropsIdxL %~ HM.alter (alterDel upId) appName))
applyActionToModifier (DeactivateProposal ui) = \p ->
    let proposal = MM.lookup (const Nothing) ui (Poll.pmActiveProps p)
    in case proposal of
           Nothing -> p
           Just ps ->
               let up = Poll.psProposal ps
                   upId = hash up
                   sv = Poll.upSoftwareVersion up
                   appName = svAppName sv
                   alterIns val Nothing   = Just $ HS.singleton val
                   alterIns val (Just hs) = Just $ HS.insert val hs
               in p & ((Poll.pmActivePropsL %~ MM.delete upId) .
                        (Poll.pmDelActivePropsIdxL %~ HM.alter (alterIns upId) appName))

applyActionToModifier (SetSlottingData sd) = Poll.pmSlottingDataL .~ (Just sd)
applyActionToModifier (SetEpochProposers hs) = Poll.pmEpochProposersL .~ (Just hs)

instance Arbitrary PollAction where
    arbitrary = oneof
        [ PutBVState <$> arbitrary <*> arbitrary
        , DelBVState <$> arbitrary
        , SetAdoptedBV <$> arbitrary
        , SetLastConfirmedSV <$> arbitrary
        , DelConfirmedSV <$> arbitrary
        , AddConfirmedProposal <$> arbitrary
        , DelConfirmedProposal <$> arbitrary
        , InsertActiveProposal <$> arbitrary
        , DeactivateProposal <$> arbitrary
        , SetSlottingData <$> arbitrary
        , SetSlottingData <$> arbitrary
        , SetEpochProposers <$> arbitrary
        ]

type PollActions = [PollAction]

applyActions
    :: Poll.PollState -> PollActions -> Property
applyActions ps actionList =
    let pollSts = fmap (actionToMonad @Poll.PurePoll) actionList
        -- 'resultModifiers' has an additional 'mempty' poll modifier up front, so we
        -- add the initial poll state at the head of 'resultPStates' to make up for that.
        resultModifiers = scanl (flip applyActionToModifier) mempty actionList
        resultPStates = ps : scanl Poll.execPurePollWithLogger ps pollSts
        newPollStates = scanl (flip Poll.modifyPollState) ps resultModifiers
    in conjoin $ zipWith (===) resultPStates newPollStates
