{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Update System core types.

module Test.Pos.Update.Arbitrary.Core
       ( genUpdatePayload
       , genUpdateVote
       , genUpdateProposal
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Test.QuickCheck (Arbitrary (..), Gen, frequency, listOf,
                                  listOf1, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                                                    genericShrink)

import           Pos.Arbitrary.Slotting ()
import           Pos.Binary.Update ()
import           Pos.Core.Update (BlockVersionModifier, SystemTag (..),
                                  UpdateData (..), UpdatePayload (..),
                                  UpdateProposal, UpdateProposalToSign (..),
                                  UpdateVote (..), mkUpdateProposalWSign,
                                  mkUpdateVote)
import           Pos.Crypto (ProtocolMagic, fakeSigner)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Update.Poll.Types (VoteState (..))

import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Crypto.Arbitrary ()
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

instance Arbitrary BlockVersionModifier where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SystemTag where
    arbitrary =
        oneof . map (pure . SystemTag) $
        [os <> arch | os <- ["win", "linux", "mac"], arch <- ["32", "64"]]
    shrink = genericShrink

genUpdateVote :: ProtocolMagic -> Gen UpdateVote
genUpdateVote pm = mkUpdateVote pm <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UpdateVote where
    arbitrary = genUpdateVote dummyProtocolMagic
    shrink = genericShrink

genUpdateProposal :: ProtocolMagic -> Gen UpdateProposal
genUpdateProposal pm = do
    upBlockVersion <- arbitrary
    upBlockVersionMod <- arbitrary
    upSoftwareVersion <- arbitrary
    upData <- HM.fromList <$> listOf1 arbitrary
    let upAttributes = mkAttributes ()
    ss <- fakeSigner <$> arbitrary
    pure $
        mkUpdateProposalWSign
            pm
            upBlockVersion
            upBlockVersionMod
            upSoftwareVersion
            upData
            upAttributes
            ss

instance Arbitrary UpdateProposal where
    arbitrary = genUpdateProposal dummyProtocolMagic
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

genUpdatePayload :: ProtocolMagic -> Gen UpdatePayload
genUpdatePayload pm =
    UpdatePayload
        <$> genMaybeUpdateProposal
        <*> listOf (genUpdateVote pm)
  where
    -- Arbitrary1 instance for Maybe uses these frequencies.
    genMaybeUpdateProposal = frequency
        [ (1, return Nothing)
        , (3, Just <$> genUpdateProposal pm)
        ]

instance Arbitrary UpdatePayload where
    arbitrary = genUpdatePayload dummyProtocolMagic
    shrink = genericShrink
