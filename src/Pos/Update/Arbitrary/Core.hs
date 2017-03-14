{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for Update System core types.

module Pos.Update.Arbitrary.Core
       (
       ) where

import           Data.DeriveTH         (derive, makeArbitrary)
import qualified Data.HashMap.Strict   as HM
import           Test.QuickCheck       (Arbitrary (..), listOf1, oneof)
import           Universum

import           Pos.Binary.Update     ()
import           Pos.Crypto            (sign, toPublic)
import           Pos.Crypto.Arbitrary  ()
import           Pos.Data.Attributes   (mkAttributes)
import           Pos.Types.Arbitrary   ()
import           Pos.Update.Core.Types (BlockVersionData (..), SystemTag, UpdateData (..),
                                        UpdatePayload (..), UpdateProposal (..),
                                        UpdateVote (..), VoteState (..), mkSystemTag)

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
        let uvSignature = sign sk (uvProposalId, uvDecision)
        return UpdateVote {..}

instance Arbitrary UpdateProposal where
    arbitrary = UpdateProposal
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (HM.fromList <$> listOf1 arbitrary)
        <*> pure (mkAttributes ())

instance Arbitrary VoteState where
    arbitrary =
        oneof $
        map pure [PositiveVote, NegativeVote, PositiveRevote, NegativeRevote]

derive makeArbitrary ''UpdateData
derive makeArbitrary ''UpdatePayload
derive makeArbitrary ''BlockVersionData
