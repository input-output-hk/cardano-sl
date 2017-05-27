-- | Arbitrary instances for Delegation types.

module Pos.Delegation.Arbitrary
       ( genDlgPayload
       ) where

import           Universum

import qualified Data.HashMap.Strict      as HM
import           Test.QuickCheck          (Arbitrary (..), Gen, listOf)

import           Pos.Binary.Core          ()
import           Pos.Core                 (EpochIndex)
import           Pos.Crypto               (ProxySecretKey (..), createProxySecretKey)
import           Pos.Delegation.Types     (DlgPayload, mkDlgPayload)
import           Pos.Types.Arbitrary.Core ()
import           Pos.Util.Util            (leftToPanic)

genDlgPayload :: EpochIndex -> Gen DlgPayload
genDlgPayload epoch =
    leftToPanic "genDlgPayload: " .
    mkDlgPayload . toList . HM.fromList . map convert <$>
    listOf genPSK
  where
    convert psk = (pskIssuerPk psk, psk)
    genPSK = createProxySecretKey <$> arbitrary <*> arbitrary <*> pure epoch

instance Arbitrary DlgPayload where
    arbitrary = arbitrary >>= genDlgPayload
