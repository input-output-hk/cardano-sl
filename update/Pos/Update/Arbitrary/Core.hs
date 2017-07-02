-- | Arbitrary instances for Update System core types.

module Pos.Update.Arbitrary.Core
       (
       ) where

import           Data.DeriveTH                     (derive, makeArbitrary)
import qualified Data.HashMap.Strict               as HM
import           Test.QuickCheck                   (Arbitrary (..), listOf1, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import           Universum

import           Pos.Binary.Update                 ()
import           Pos.Core.Arbitrary                ()
import           Pos.Crypto                        (SignTag (SignUSVote), fakeSigner,
                                                    sign, toPublic)
import           Pos.Crypto.Arbitrary              ()
import           Pos.Data.Attributes               (mkAttributes)
import           Pos.Update.Core.Types             (BlockVersionModifier (..), SystemTag,
                                                    UpdateData (..), UpdatePayload (..),
                                                    UpdateProposal (..),
                                                    UpdateProposalToSign, UpdateVote (..),
                                                    VoteState (..), mkSystemTag,
                                                    mkUpdateProposalWSign)

derive makeArbitrary ''BlockVersionModifier

instance Arbitrary SystemTag where
    arbitrary =
        oneof $
        map (pure . fromMaybe onFail) [mkSystemTag "win64", mkSystemTag "mac32"]
      where
        onFail = error "instance Arbitrary SystemTag: disaster"

instance Arbitrary UpdateVote where
    arbitrary = do
        sk <- arbitrary
        let uvKey = toPublic sk
        uvProposalId <- arbitrary
        uvDecision <- arbitrary
        let uvSignature = sign SignUSVote sk (uvProposalId, uvDecision)
        return UpdateVote {..}

instance Arbitrary UpdateProposal where
    arbitrary = do
        upBlockVersion <- arbitrary
        upBlockVersionMod <- arbitrary
        upSoftwareVersion <- arbitrary
        upData <- HM.fromList <$> listOf1 arbitrary
        let upAttributes = mkAttributes ()
        ss <- fakeSigner <$> arbitrary
        let onFailure = error . mappend "arbitrary @UpdateProposal failed: "
        either onFailure pure $
            mkUpdateProposalWSign
                upBlockVersion
                upBlockVersionMod
                upSoftwareVersion
                upData
                upAttributes
                ss

instance Arbitrary UpdateProposalToSign where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VoteState where
    arbitrary =
        oneof $
        map pure [PositiveVote, NegativeVote, PositiveRevote, NegativeRevote]

derive makeArbitrary ''UpdateData
derive makeArbitrary ''UpdatePayload
