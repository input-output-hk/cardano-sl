{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Update System core types.

module Pos.Arbitrary.Update.Core
       ( genUpdatePayload
       , genUpdateVote
       , genUpdateProposal
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Test.QuickCheck (Arbitrary (..), Gen, frequency, listOf, listOf1, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core ()
import           Pos.Arbitrary.Slotting ()
import           Pos.Binary.Update ()
import           Pos.Core.Configuration (HasProtocolMagic, protocolMagic)
import           Pos.Core.Update (BlockVersionModifier, SystemTag (..), UpdateData (..),
                                  UpdatePayload (..), UpdateProposal (..),
                                  UpdateProposalToSign (..), UpdateVote (..), mkUpdateProposalWSign,
                                  mkUpdateVote)
import           Pos.Crypto (ProtocolMagic, fakeSigner)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Update.Poll.Types (VoteState (..))

import           Test.Pos.Crypto.Arbitrary ()

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

instance HasProtocolMagic => Arbitrary UpdateVote where
    arbitrary = genUpdateVote protocolMagic
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

instance HasProtocolMagic => Arbitrary UpdateProposal where
    arbitrary = genUpdateProposal protocolMagic
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
genUpdatePayload pm = UpdatePayload <$> genMaybeUpdateProposal <*> listOf (genUpdateVote pm)
  where
    -- Arbitrary1 instance for Maybe uses these frequencies.
    genMaybeUpdateProposal = frequency
        [ (1, return Nothing)
        , (3, Just <$> genUpdateProposal pm)
        ]

instance HasProtocolMagic => Arbitrary UpdatePayload where
    arbitrary = genUpdatePayload protocolMagic
    shrink = genericShrink
