{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Delegation types.

module Test.Pos.Chain.Delegation.Arbitrary
       ( genDlgPayload
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Test.QuickCheck (Arbitrary (..), Gen, listOf)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Pos.Chain.Delegation (DlgPayload (..), DlgUndo (..),
                     HeavyDlgIndex (..), LightDlgIndices (..))
import           Pos.Core (EpochIndex)
import           Pos.Crypto (ProtocolMagic, ProxySecretKey (..), createPsk)

import           Test.Pos.Core.Arbitrary ()

genDlgPayload :: ProtocolMagic -> EpochIndex -> Gen DlgPayload
genDlgPayload pm epoch =
    UnsafeDlgPayload . toList . HM.fromList . map convert <$> listOf genPSK
  where
    convert psk = (pskIssuerPk psk, psk)
    genPSK = createPsk pm <$> arbitrary <*> arbitrary <*> pure (HeavyDlgIndex epoch)

instance Arbitrary DlgPayload where
    arbitrary = do
        pm <- arbitrary
        ei <- arbitrary
        genDlgPayload pm ei
    shrink = genericShrink

instance Arbitrary DlgUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary HeavyDlgIndex where
    arbitrary = HeavyDlgIndex <$> arbitrary
    shrink = genericShrink

instance Arbitrary LightDlgIndices where
    arbitrary = do
        l <- arbitrary
        r <- arbitrary
        pure $ LightDlgIndices $ if r >= l then (l,r) else (r,l)
    shrink = genericShrink
