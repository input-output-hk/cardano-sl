{-# LANGUAGE TypeApplications #-}

-- | This module tests SafeCopy instances.

module Test.Pos.Ssc.DynamicState.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Ssc.DynamicState  as DS

import           Test.Pos.Util         (safeCopyEncodeDecode)

spec :: Spec
spec = describe "DynamicState" $ do
    describe "SafeCopy instances" $ do
        prop "Commitment" (safeCopyEncodeDecode @DS.Commitment)
        prop "CommitmentSignature" (safeCopyEncodeDecode @DS.CommitmentSignature)
        prop "SignedCommitment" (safeCopyEncodeDecode @DS.SignedCommitment)
        prop "Opening" (safeCopyEncodeDecode @DS.Opening)
        prop "VssCertificate" (safeCopyEncodeDecode @DS.VssCertificate)
