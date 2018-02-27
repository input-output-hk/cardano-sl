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
import           Pos.Core (EpochIndex)
import           Pos.Crypto (ProtocolMagic, ProxySecretKey (..), createPsk)
import           Pos.Delegation.Types (DlgPayload (..), DlgUndo)

genDlgPayload :: Arbitrary ProtocolMagic => EpochIndex -> Gen DlgPayload
genDlgPayload epoch =
    UnsafeDlgPayload . toList . HM.fromList . map convert <$>
    listOf genPSK
  where
    convert psk = (pskIssuerPk psk, psk)
    genPSK = createPsk <$> arbitrary <*> arbitrary <*> arbitrary <*> pure epoch

instance Arbitrary ProtocolMagic => Arbitrary DlgPayload where
    arbitrary = arbitrary >>= genDlgPayload
    shrink = genericShrink

instance Arbitrary ProtocolMagic => Arbitrary DlgUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink
