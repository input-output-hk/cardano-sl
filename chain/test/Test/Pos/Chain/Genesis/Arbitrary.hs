{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for core.

module Test.Pos.Chain.Genesis.Arbitrary
       ( genGenesisData
       , genGenesisProtocolConstants
       ) where

import           Universum

import           Data.Time.Units (Second, convertUnit)
import           Test.QuickCheck (Arbitrary (..), Gen, choose, sized, suchThat)

import           Pos.Chain.Delegation (HeavyDlgIndex (..))
import           Pos.Chain.Genesis
import           Pos.Chain.Ssc (mkVssCertificatesMapLossy)
import           Pos.Chain.Update (BlockVersionData (..))
import           Pos.Core (Timestamp (..), TxFeePolicy (..), maxCoinVal)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..),
                     VssMaxTTL (..), VssMinTTL (..))
import           Pos.Crypto (ProtocolMagic, createPsk, toPublic)
import           Pos.Util.Util (leftToPanic)

import           Test.Pos.Chain.Ssc.Arbitrary ()
import           Test.Pos.Chain.Update.Arbitrary ()
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)
import           Test.Pos.Util.QuickCheck.Arbitrary (nonrepeating)

instance Arbitrary TestnetBalanceOptions where
    arbitrary = do
        -- We have at least 2 owned addresses in system so we can send
        -- transactions in block-gen/tests.
        tboPoors <- choose (1, 100)
        tboRichmen <- choose (1, 12)
        tboTotalBalance <- choose (1000, maxCoinVal)
        tboRichmenShare <- choose (0.55, 0.996)
        let tboUseHDAddresses = False
        return TestnetBalanceOptions {..}

instance Arbitrary FakeAvvmOptions where
    arbitrary = do
        faoCount <- choose (0, 10)
        faoOneBalance <- choose (5, 30)
        return FakeAvvmOptions {..}

instance Arbitrary GenesisDelegation where
    arbitrary =
        leftToPanic "arbitrary@GenesisDelegation" . mkGenesisDelegation <$> do
            secretKeys <- sized (nonrepeating . min 10) -- we generate at most tens keys,
                                                        -- because 'nonrepeating' fails when
                                                        -- we want too many items, because
                                                        -- life is hard
            return $
                case secretKeys of
                    []                 -> []
                    (delegate:issuers) -> mkCert (toPublic delegate) <$> issuers
      where
        mkCert delegatePk issuer = createPsk dummyProtocolMagic issuer delegatePk (HeavyDlgIndex 0)

instance Arbitrary GenesisWStakeholders where
    arbitrary = GenesisWStakeholders <$> arbitrary

instance Arbitrary GenesisAvvmBalances where
    arbitrary = GenesisAvvmBalances <$> arbitrary

instance Arbitrary GenesisNonAvvmBalances where
    arbitrary = GenesisNonAvvmBalances <$> arbitrary


instance Arbitrary ProtocolConstants where
    arbitrary = do
        vssA <- arbitrary
        vssB <- arbitrary
        let (vssMin, vssMax) = if vssA > vssB
                               then (VssMinTTL vssB, VssMaxTTL vssA)
                               else (VssMinTTL vssA, VssMaxTTL vssB)
        ProtocolConstants <$> choose (1, 20000) <*> pure vssMin <*> pure vssMax

instance Arbitrary GenesisProtocolConstants where
    arbitrary = genGenesisProtocolConstants arbitrary

genGenesisProtocolConstants
    :: Gen ProtocolMagic
    -> Gen GenesisProtocolConstants
genGenesisProtocolConstants genPM =
    genesisProtocolConstantsFromProtocolConstants <$> arbitrary <*> genPM

instance Arbitrary GenesisData where
    arbitrary = genGenesisData arbitrary

genGenesisData :: Gen GenesisProtocolConstants -> Gen GenesisData
genGenesisData genGPC =
    GenesisData
        <$> arbitrary <*> arbitrary <*> arbitraryStartTime
        <*> arbitraryVssCerts <*> arbitrary <*> arbitraryBVD
        <*> genGPC <*> arbitrary <*> arbitrary
  where
    -- System start time should be multiple of a second.
    arbitraryStartTime = Timestamp . convertUnit @Second <$> arbitrary
    -- Unknown tx fee policy in genesis is not ok.
    arbitraryBVD = arbitrary `suchThat` hasKnownFeePolicy
    hasKnownFeePolicy BlockVersionData {bvdTxFeePolicy = TxFeePolicyTxSizeLinear {}} = True
    hasKnownFeePolicy _ = False
    arbitraryVssCerts = mkVssCertificatesMapLossy <$> arbitrary
