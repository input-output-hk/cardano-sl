
-- | This module tests Binary instances.

module Test.Pos.Ssc.DynamicState.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Ssc.DynamicState  as DS

import           Test.Pos.Util         (binaryEncodeDecode)

spec :: Spec
spec = describe "DynamicState" $ do
    describe "Binary instances" $ do
        prop "Commitment" (binaryEncodeDecode @DS.Commitment)
        prop "CommitmentSignature" (binaryEncodeDecode @DS.CommitmentSignature)
        prop "SignedCommitment" (binaryEncodeDecode @DS.SignedCommitment)
        prop "Opening" (binaryEncodeDecode @DS.Opening)
        prop "VssCertificate" (binaryEncodeDecode @DS.VssCertificate)
