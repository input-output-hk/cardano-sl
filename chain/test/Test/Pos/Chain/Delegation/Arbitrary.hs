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

import           Pos.Chain.Delegation (DlgPayload (..), DlgUndo (..))
import           Pos.Core (EpochIndex)
import           Pos.Core.Delegation (HeavyDlgIndex (..))
import           Pos.Crypto (ProtocolMagic, ProxySecretKey (..), createPsk)

import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

genDlgPayload :: ProtocolMagic -> EpochIndex -> Gen DlgPayload
genDlgPayload pm epoch =
    UnsafeDlgPayload . toList . HM.fromList . map convert <$> listOf genPSK
  where
    convert psk = (pskIssuerPk psk, psk)
    genPSK = createPsk pm <$> arbitrary <*> arbitrary <*> pure (HeavyDlgIndex epoch)

instance Arbitrary DlgPayload where
    arbitrary = arbitrary >>= genDlgPayload dummyProtocolMagic
    shrink = genericShrink

instance Arbitrary DlgUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink
