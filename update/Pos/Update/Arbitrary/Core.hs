-- | Arbitrary instances for Update System core types.

module Pos.Update.Arbitrary.Core
       (
       ) where

import           Universum

import           Data.DeriveTH                     (derive, makeArbitrary)
import qualified Data.HashMap.Strict               as HM
import           Test.QuickCheck                   (Arbitrary (..), listOf1, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Binary.Update                 ()
import           Pos.Core.Arbitrary                ()
import           Pos.Crypto                        (SignTag (SignUSVote), fakeSigner,
                                                    sign, toPublic)
import           Pos.Crypto.Arbitrary              ()
import           Pos.Data.Attributes               (mkAttributes)
import           Pos.Update.Core.Types             (BlockVersionData (..),
                                                    BlockVersionModifier, SystemTag,
                                                    UpdateData (..), UpdatePayload (..),
                                                    UpdateProposal (..),
                                                    UpdateProposalToSign (..),
                                                    UpdateVote (..), VoteState (..),
                                                    mkSystemTag, mkUpdateProposalWSign)

instance Arbitrary BlockVersionModifier where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SystemTag where
    arbitrary =
        oneof $
        map (pure . fromMaybe onFail) [mkSystemTag "win64", mkSystemTag "mac32"]
      where
        onFail = error "instance Arbitrary SystemTag: disaster"
    shrink = genericShrink

instance Arbitrary UpdateVote where
    arbitrary = do
        sk <- arbitrary
        let uvKey = toPublic sk
        uvProposalId <- arbitrary
        uvDecision <- arbitrary
        let uvSignature = sign SignUSVote sk (uvProposalId, uvDecision)
        return UpdateVote {..}
    shrink = genericShrink

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
    shrink = genericShrink

instance Arbitrary UpdateProposalToSign where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VoteState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary UpdateData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary UpdatePayload where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlockVersionData where
    arbitrary = genericArbitrary
    shrink = genericShrink
