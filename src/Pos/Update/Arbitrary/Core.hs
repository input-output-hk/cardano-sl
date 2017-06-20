-- | Arbitrary instances for Update System core types.

module Pos.Update.Arbitrary.Core
       (
       ) where

import           Data.DeriveTH         (derive, makeArbitrary)
import qualified Data.HashMap.Strict   as HM
import           Test.QuickCheck       (Arbitrary (..), listOf1, oneof)
import           Universum

import           Pos.Binary.Update     ()
import           Pos.Crypto            (SignTag (SignUSVote), fakeSigner, sign, toPublic)
import           Pos.Crypto.Arbitrary  ()
import           Pos.Data.Attributes   (mkAttributes)
import           Pos.Types.Arbitrary   ()
import           Pos.Update.Core.Types (BlockVersionData (..), SystemTag, UpdateData (..),
                                        UpdatePayload (..), UpdateProposal (..),
                                        UpdateVote (..), VoteState (..), mkSystemTag,
                                        mkUpdateProposalWSign)

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
        upBlockVersionData <- arbitrary
        upSoftwareVersion <- arbitrary
        upData <- HM.fromList <$> listOf1 arbitrary
        let upAttributes = mkAttributes ()
        ss <- fakeSigner <$> arbitrary
        let onFailure = error . mappend "arbitrary @UpdateProposal failed: "
        either onFailure pure $
            mkUpdateProposalWSign
                upBlockVersion
                upBlockVersionData
                upSoftwareVersion
                upData
                upAttributes
                ss

instance Arbitrary VoteState where
    arbitrary =
        oneof $
        map pure [PositiveVote, NegativeVote, PositiveRevote, NegativeRevote]

derive makeArbitrary ''UpdateData
derive makeArbitrary ''UpdatePayload
derive makeArbitrary ''BlockVersionData
