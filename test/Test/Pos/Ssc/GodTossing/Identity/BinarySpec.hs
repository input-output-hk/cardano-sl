
-- | This module tests Binary instances.

module Test.Pos.Ssc.GodTossing.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Ssc.GodTossing    as GT

import           Test.Pos.Util         (binaryEncodeDecode)

spec :: Spec
spec = describe "GodTossing" $ do
    describe "Bi instances" $ do
        prop "Commitment" (binaryEncodeDecode @GT.Commitment)
        prop "CommitmentSignature" (binaryEncodeDecode @GT.CommitmentSignature)
        prop "SignedCommitment" (binaryEncodeDecode @GT.SignedCommitment)
        prop "Opening" (binaryEncodeDecode @GT.Opening)
        prop "VssCertificate" (binaryEncodeDecode @GT.VssCertificate)
