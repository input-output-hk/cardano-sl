-- | This module tests SafeCopy instances.

module Test.Pos.Ssc.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import qualified Pos.Core.Ssc as Ssc

import           Test.Pos.Binary.Helpers (safeCopyTest)
import           Test.Pos.Infra.Arbitrary.Ssc ()

spec :: Spec
spec = describe "Ssc" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @Ssc.Commitment
        safeCopyTest @Ssc.CommitmentSignature
        safeCopyTest @Ssc.SignedCommitment
        safeCopyTest @Ssc.Opening
        safeCopyTest @Ssc.SscPayload
        safeCopyTest @Ssc.SscProof
