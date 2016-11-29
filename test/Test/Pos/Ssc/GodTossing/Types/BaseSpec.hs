-- | Specification of Pos.Ssc.GodTossing.Types.Base

module Test.Pos.Ssc.GodTossing.Types.BaseSpec
       ( spec
       ) where

import           Pos.Crypto            (SecretKey, sign, toPublic)
import           Pos.Ssc.GodTossing    (Commitment, CommitmentOpening (..),
                                        verifyCommitment, verifyCommitmentSignature,
                                        verifyOpening)
import           Pos.Types             (EpochIndex)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

spec :: Spec
spec = describe "Ssc.GodTossing.Base" $ do
    describe "verifyCommitment" $ do
        prop description_verifiesOkComm verifiesOkComm
    describe "verifyCommitmentSignature" $ do
        prop description_verifiesOkCommSig verifiesOkCommSig
    describe "verifyOpening" $ do
        prop description_verifiesOkOpening verifiesOkOpening
  where
    description_verifiesOkComm =
        "successfully verifies a correct commitment, and fails to verify an incorrect\
        \ commitment"
    description_verifiesOkCommSig =
        "successfully verifies a signed commitment for a given epoch and secret key"
    description_verifiesOkOpening =
        "successfully verifies that an opening corresponding to the given commitment\
        \ does indeed belong to it"

verifiesOkComm :: CommitmentOpening -> Bool
verifiesOkComm CommitmentOpening{..} =
    verifyCommitment coCommitment

verifiesOkCommSig :: SecretKey -> Commitment -> EpochIndex -> Bool
verifiesOkCommSig sk comm epoch =
    let commSig = (comm, sign sk (epoch, comm))
        pk = toPublic sk
    in verifyCommitmentSignature pk epoch commSig

verifiesOkOpening :: CommitmentOpening -> Bool
verifiesOkOpening CommitmentOpening{..} =
    verifyOpening coCommitment coOpening
