{-# LANGUAGE TypeApplications #-}

-- | This module tests MessagePack instances.

module Test.Pos.Ssc.DynamicState.Identity.MessagePackSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import qualified Pos.Ssc.DynamicState  as DS

import           Test.Pos.Util         (msgPackEncodeDecode)

spec :: Spec
spec = describe "Types" $ do
    describe "MessagePack instances" $ do
        prop "Commitment" (msgPackEncodeDecode @DS.Commitment)
        prop "CommitmentSignature" (msgPackEncodeDecode @DS.CommitmentSignature)
        prop "SignedCommitment" (msgPackEncodeDecode @DS.SignedCommitment)
        prop "Opening" (msgPackEncodeDecode @DS.Opening)
        prop "VssCertificate" (msgPackEncodeDecode @DS.VssCertificate)
