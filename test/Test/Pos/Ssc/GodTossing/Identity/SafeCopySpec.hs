-- | This module tests SafeCopy instances.

module Test.Pos.Ssc.GodTossing.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec         (Spec, describe)
import           Universum

import qualified Pos.Ssc.GodTossing as GT

import           Test.Pos.Util      (safeCopyTest)

spec :: Spec
spec = describe "GodTossing" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @GT.Commitment
        safeCopyTest @GT.CommitmentSignature
        safeCopyTest @GT.SignedCommitment
        safeCopyTest @GT.Opening
        safeCopyTest @GT.VssCertificate
        safeCopyTest @GT.GtPayload
        safeCopyTest @GT.GtProof
        safeCopyTest @GT.GtGlobalState
