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
import           Pos.Core (EpochIndex, HasConfiguration, HeavyDlgIndex (..))
import           Pos.Crypto (ProxySecretKey (..), createPsk)
import           Pos.Delegation.Types (DlgPayload (..), DlgUndo (..))

genDlgPayload :: HasConfiguration => EpochIndex -> Gen DlgPayload
genDlgPayload epoch =
    UncheckedDlgPayload . toList . HM.fromList . map convert <$> listOf genPSK
  where
    convert psk = (pskIssuerPk psk, psk)
    genPSK = createPsk <$> arbitrary <*> arbitrary <*> pure (HeavyDlgIndex epoch)

instance HasConfiguration => Arbitrary DlgPayload where
    arbitrary = arbitrary >>= genDlgPayload
    shrink = genericShrink

instance HasConfiguration => Arbitrary DlgUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink
