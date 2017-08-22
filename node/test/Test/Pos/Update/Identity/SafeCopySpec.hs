-- | This module tests SafeCopy instances.

module Test.Pos.Update.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec    (Spec, describe)
import           Universum

import qualified Pos.Update    as U

import           Test.Pos.Util (safeCopyTest)

spec :: Spec
spec = describe "Update system" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @U.UpdateProposal
        safeCopyTest @U.UpdateVote
        safeCopyTest @U.UpdateData
        safeCopyTest @U.SystemTag
