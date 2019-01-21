{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Chain.Update.Arbitrary
       ( genUpdatePayload
       , genUpdateVote
       , genUpdateProposal
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Test.QuickCheck (Arbitrary (..), Gen, elements, frequency,
                     listOf, listOf1)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Chain.Update (BlockVersion (..), BlockVersionData (..),
                     BlockVersionModifier, ConsensusEra,
                     ObftConsensusStrictness, SoftforkRule (..),
                     SystemTag (..), UpdateData (..), UpdatePayload (..),
                     UpdateProposal, UpdateProposalToSign (..),
                     UpdateVote (..), VoteState (..), mkUpdateProposalWSign,
                     mkUpdateVote)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Crypto (ProtocolMagic, fakeSigner)

import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Core.Arbitrary.Slotting ()
import           Test.Pos.Crypto.Arbitrary ()
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)


instance Arbitrary SoftforkRule where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlockVersionData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlockVersion where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BlockVersionModifier where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ConsensusEra where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ObftConsensusStrictness where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SystemTag where
    arbitrary =
        elements . fmap SystemTag $
        (<>) <$> ["win", "linux", "mac"] <*> ["32", "64"]
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
