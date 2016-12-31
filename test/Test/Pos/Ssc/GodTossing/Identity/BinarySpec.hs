-- | This module tests Binary instances for Pos.Ssc.GodTossing types

module Test.Pos.Ssc.GodTossing.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec         (Spec, describe)
import           Universum

import qualified Pos.Ssc.GodTossing as GT

import           Test.Pos.Util      (binaryTest)

spec :: Spec
spec = describe "GodTossing" $ do
    describe "Bi instances" $ do
        binaryTest @GT.Commitment
        binaryTest @GT.Opening
        binaryTest @GT.VssCertificate
        binaryTest @GT.GtProof
        binaryTest @GT.GtPayload
        binaryTest @GT.MsgTag
        binaryTest @GT.InvMsg
        binaryTest @GT.ReqMsg
        binaryTest @GT.DataMsg
        binaryTest @GT.GtSecretStorage
