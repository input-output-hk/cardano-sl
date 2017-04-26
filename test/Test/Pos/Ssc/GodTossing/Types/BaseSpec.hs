-- | Specification of Pos.Ssc.GodTossing.Types.Base

module Test.Pos.Ssc.GodTossing.Types.BaseSpec
       ( spec
       ) where

import           Pos.Binary            ()
import           Pos.Crypto            (SecretKey, SignTag (SignCommitment), sign,
                                        toPublic)
import           Pos.Ssc.GodTossing    (BadCommAndOpening (..), BadCommitment (..),
                                        BadSignedCommitment (..), Commitment,
                                        CommitmentOpening (..), verifyCommitment,
                                        verifyCommitmentSignature, verifyOpening)
import           Pos.Types             (EpochIndex)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

spec :: Spec
spec = describe "Ssc.GodTossing.Base" $ do
    describe "verifyCommitment" $ do
        prop description_verifiesOkComm verifiesOkComm
        prop description_notVerifiesBadComm notVerifiesBadComm
    describe "verifyCommitmentSignature" $ do
        prop description_verifiesOkCommSig verifiesOkCommSig
        prop description_notVerifiesBadSig notVerifiesBadCommSig
    describe "verifyOpening" $ do
        prop description_verifiesOkOpening verifiesOkOpening
        prop description_notVerifiesBadOpening notVerifiesBadOpening
  where
    description_verifiesOkComm =
        "successfully verifies a correct commitment commitment"
    description_notVerifiesBadComm =
        "unsuccessfully verifies an incorrect commitment"
    description_verifiesOkCommSig =
        "successfully verifies a signed commitment for a given epoch and secret key"
    description_notVerifiesBadSig =
        "unsuccessfully verifies a commitment signature and a mismatching epoch"
    description_verifiesOkOpening =
        "successfully verifies that an opening corresponding to the given commitment\
\ does indeed belong to it"
    description_notVerifiesBadOpening =
        "unsuccessfully verifies a mismatching commitment and opening"

verifiesOkComm :: CommitmentOpening -> Bool
verifiesOkComm CommitmentOpening{..} =
    verifyCommitment coCommitment

notVerifiesBadComm :: BadCommitment -> Bool
notVerifiesBadComm (getBadComm -> badComm) =
    not . verifyCommitment $ badComm

verifiesOkCommSig :: SecretKey -> Commitment -> EpochIndex -> Bool
verifiesOkCommSig sk comm epoch =
    let commSig = (toPublic sk, comm, sign SignCommitment sk (epoch, comm))
    in verifyCommitmentSignature epoch commSig

notVerifiesBadCommSig :: BadSignedCommitment -> EpochIndex -> Bool
notVerifiesBadCommSig (getBadSignedC -> badSignedComm) epoch =
    not $ verifyCommitmentSignature epoch badSignedComm

verifiesOkOpening :: CommitmentOpening -> Bool
verifiesOkOpening CommitmentOpening{..} =
    verifyOpening coCommitment coOpening

notVerifiesBadOpening :: BadCommAndOpening -> Bool
notVerifiesBadOpening (getBadCAndO -> badCommsAndOp) =
    not . uncurry verifyOpening $ badCommsAndOp
