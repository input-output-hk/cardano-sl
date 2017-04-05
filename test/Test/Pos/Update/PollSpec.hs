-- | Specification for submodules of Pos.Update.Poll

module Test.Pos.Update.PollSpec
       ( spec
       ) where

import           Data.Default             (def)

import qualified Pos.Update.Poll.Modifier as Poll
import qualified Pos.Update.Poll.Types    as Poll

import           Test.Hspec               (Spec, describe)
import           Test.Hspec.QuickCheck    (prop)
import           Test.Pos.Util            (formsSemigroup)
import           Test.QuickCheck          (Property, (===))
import           Universum

spec :: Spec
spec = describe "Poll" $ do
    describe "modifyPollModifier" $ do
        prop
            "poll modifiers form a commutative monoid under 'modifyPollModifier'"
            modifyPollFormsMonoid
        prop
            "modifying an initially empty poll modifier is the same as starting with\
            \ the second modifier"
            defIsLeftIdentity
        prop
            "modifying a poll modifier with the default (empty) modifier only discards\
            \ the epoch proposers in the old modifier"
            defIsNotRightIdentity

modifyPollFormsMonoid
    :: Poll.PollModifier
    -> Poll.PollModifier
    -> Poll.PollModifier
    -> Property
modifyPollFormsMonoid = formsSemigroup

defIsLeftIdentity :: Poll.PollModifier -> Property
defIsLeftIdentity poll = def `Poll.modifyPollModifier` poll === poll

defIsNotRightIdentity :: Poll.PollModifier -> Property
defIsNotRightIdentity poll =
    poll `Poll.modifyPollModifier` def === poll {Poll.pmEpochProposers = Nothing}
