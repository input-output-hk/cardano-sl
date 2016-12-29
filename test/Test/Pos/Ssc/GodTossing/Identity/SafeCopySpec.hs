{-# LANGUAGE TypeApplications #-}

-- | This module tests SafeCopy instances.

module Test.Pos.Ssc.GodTossing.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Ssc.GodTossing    as GT

import           Test.Pos.Util         (safeCopyEncodeDecode)

spec :: Spec
spec = describe "GodTossing" $ do
    describe "SafeCopy instances" $ do
        prop "Commitment"          (safeCopyEncodeDecode @GT.Commitment)
        prop "CommitmentSignature" (safeCopyEncodeDecode @GT.CommitmentSignature)
        prop "SignedCommitment"    (safeCopyEncodeDecode @GT.SignedCommitment)
        prop "Opening"             (safeCopyEncodeDecode @GT.Opening)
        prop "VssCertificate"      (safeCopyEncodeDecode @GT.VssCertificate)
        prop "GtPayload"           (safeCopyEncodeDecode @GT.GtPayload)
        prop "GtProof"             (safeCopyEncodeDecode @GT.GtProof)
        prop "GtGlobalState"       (safeCopyEncodeDecode @GT.GtGlobalState)
