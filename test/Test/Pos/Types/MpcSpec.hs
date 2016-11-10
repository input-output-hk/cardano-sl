-- | Specification of Pos.Types.Mpc

module Test.Pos.Types.MpcSpec
       ( spec
       ) where

import qualified Data.ByteString.Char8 as BS (pack)
import           Pos.Constants         (ftsSeedLength)
import           Pos.Crypto            (SecretKey, sign, toPublic)
import           Pos.Ssc.DynamicState  (Commitment, CommitmentOpening (..),
                                        verifyCommitment, verifyCommitmentSignature,
                                        verifyOpening)
import           Pos.Types             (EpochIndex, FtsSeed (..), xorSharedSeed)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Universum

spec :: Spec
spec = describe "Types.Mpc" $ do
    describe "verifyCommitment" $ do
        prop description_verifiesOkComm verifiesOkComm
    describe "verifyCommitmentSignature" $ do
        prop description_verifiesOkCommSig verifiesOkCommSig
    describe "verifyOpening" $ do
        prop description_verifiesOkOpening verifiesOkOpening
    describe "xorSharedSeed" $ do
        prop description_xorFormsAbelianGroup xorFormsAbelianGroup
  where
    description_verifiesOkComm =
        "successfully verifies a correct commitment, and fails to verify an incorrect \
        \ commitment"
    description_verifiesOkCommSig =
        "successfully verifies a signed commitment for a given epoch and secret key"
    description_verifiesOkOpening =
        "successfully verifies that an opening corresponding to the given commitment \
        \ does indeed belong to it"
    description_xorFormsAbelianGroup =
        "under the xorFtsSeed operation, the set of ftsSeedLength-byte FtsSeeds is an \
        \ abelian group"

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

xorFormsAbelianGroup :: FtsSeed -> FtsSeed -> FtsSeed -> Bool
xorFormsAbelianGroup fts1 fts2 fts3 =
    let -- | xorFtsSeed is (<>) because of our Monoid instance
        isAssociative =
            let assoc1 = (fts1 <> fts2) <> fts3
                assoc2 = fts1 <> (fts2 <> fts3)
            in assoc1 == assoc2
        hasIdentity =
            let id1 = mempty <> fts1
                id2 = fts1 <> mempty
            in (fts1 == id1) && (fts1 == id2)
        hasInverses =
            let inv1 = fts1 <> fts2
                inv2 = inv1 <> fts2
                inv3 = fts1 <> inv1
            in inv2 == fts1 && inv3 == fts2
        isCommutative =
            let comm1 = fts1 <> fts2
                comm2 = fts2 <> fts1
            in comm1 == comm2
    in isAssociative && hasIdentity && hasInverses && isCommutative
