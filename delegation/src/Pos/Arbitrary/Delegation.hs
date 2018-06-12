{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Delegation types.

module Pos.Arbitrary.Delegation
       ( genDlgPayload
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Test.QuickCheck (Arbitrary (..), Gen, listOf)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Core ()
import           Pos.Binary.Core ()
import           Pos.Core (EpochIndex, HeavyDlgIndex (..))
import           Pos.Crypto (ProtocolMagic, ProxySecretKey (..), createPsk)
import           Pos.Delegation.Types (DlgPayload (..), DlgUndo (..))

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
