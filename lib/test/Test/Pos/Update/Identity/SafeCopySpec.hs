-- | This module tests SafeCopy instances.

module Test.Pos.Update.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import           Pos.SafeCopy ()
import qualified Pos.Update as U

import           Test.Pos.Binary.Helpers (safeCopyTest)
import           Test.Pos.Configuration (withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "Update system" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @U.UpdateProposal
        safeCopyTest @U.UpdateVote
        safeCopyTest @U.UpdateData
        safeCopyTest @U.SystemTag
