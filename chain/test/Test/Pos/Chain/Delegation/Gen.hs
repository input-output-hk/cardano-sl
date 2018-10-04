module Test.Pos.Chain.Delegation.Gen
       ( genDlgPayload
       , genHeavyDlgIndex
       , genLightDlgIndices
       , genProxySKBlockInfo
       , genProxySKHeavy
       ) where

import           Universum

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Chain.Delegation (DlgPayload (..), HeavyDlgIndex (..),
                     LightDlgIndices (..), ProxySKBlockInfo, ProxySKHeavy)
import           Pos.Crypto (ProtocolMagic, safeCreatePsk)

import           Test.Pos.Core.Gen (genEpochIndex)
import           Test.Pos.Crypto.Gen (genPublicKey, genSafeSigner)

genDlgPayload :: ProtocolMagic -> Gen DlgPayload
genDlgPayload pm =
    UnsafeDlgPayload <$> Gen.list (Range.linear 0 5) (genProxySKHeavy pm)

genHeavyDlgIndex :: Gen HeavyDlgIndex
genHeavyDlgIndex = HeavyDlgIndex <$> genEpochIndex

genLightDlgIndices :: Gen LightDlgIndices
genLightDlgIndices =
    LightDlgIndices <$> ((,) <$> genEpochIndex <*> genEpochIndex)

genProxySKBlockInfo :: ProtocolMagic -> Gen ProxySKBlockInfo
genProxySKBlockInfo pm = Gen.maybe $ do
    pSKHeavy <- genProxySKHeavy pm
    pubKey <- genPublicKey
    pure (pSKHeavy,pubKey)

genProxySKHeavy :: ProtocolMagic -> Gen ProxySKHeavy
genProxySKHeavy pm =
    safeCreatePsk pm
        <$> genSafeSigner
        <*> genPublicKey
        <*> genHeavyDlgIndex
