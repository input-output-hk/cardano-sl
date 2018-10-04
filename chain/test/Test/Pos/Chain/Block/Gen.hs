module Test.Pos.Chain.Block.Gen
       ( genBlockBodyAttributes
       , genBlockHeader
       , genBlockHeaderAttributes
       , genBlockSignature
       , genGenesisBlockHeader
       , genGenesisBody
       , genGenesisConsensusData
       , genGenesisProof
       , genHeaderHash
       , genMainBlockHeader
       , genMainBody
       , genMainConsensusData
       , genMainExtraBodyData
       , genMainExtraHeaderData
       , genMainProof
       , genMainToSign
       ) where

import           Universum

import           Data.Coerce (coerce)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

import           Pos.Chain.Block (BlockBodyAttributes, BlockHeader (..),
                     BlockHeaderAttributes, BlockSignature (..),
                     GenesisBlockHeader, GenesisBody (..),
                     GenesisConsensusData (..), GenesisProof (..), HeaderHash,
                     MainBlockHeader, MainBody (..), MainConsensusData (..),
                     MainExtraBodyData (..), MainExtraHeaderData (..),
                     MainProof (..), MainToSign (..), mkGenesisHeader,
                     mkMainHeaderExplicit)
import           Pos.Core (SlotCount)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Crypto (ProtocolMagic)

import           Test.Pos.Chain.Delegation.Gen (genDlgPayload, genHeavyDlgIndex,
                     genLightDlgIndices)
import           Test.Pos.Chain.Ssc.Gen (genSscPayload, genSscProof)
import           Test.Pos.Chain.Txp.Gen (genTxPayload, genTxProof)
import           Test.Pos.Chain.Update.Gen (genBlockVersion, genSoftwareVersion,
                     genUpdatePayload, genUpdateProof)
import           Test.Pos.Core.Gen (genChainDifficulty, genEpochIndex,
                     genSlotId, genSlotLeaders, genTextHash)
import           Test.Pos.Crypto.Gen (genAbstractHash, genProxySignature,
                     genPublicKey, genSecretKey, genSignature)

genBlockBodyAttributes :: Gen BlockBodyAttributes
genBlockBodyAttributes = pure $ mkAttributes ()

genBlockHeader :: ProtocolMagic -> SlotCount -> Gen BlockHeader
genBlockHeader pm epochSlots =
    Gen.choice [ BlockHeaderGenesis <$> genGenesisBlockHeader pm epochSlots
               , BlockHeaderMain <$> genMainBlockHeader pm epochSlots
               ]

genBlockHeaderAttributes :: Gen BlockHeaderAttributes
genBlockHeaderAttributes = pure $ mkAttributes ()

genBlockSignature :: ProtocolMagic -> SlotCount -> Gen BlockSignature
genBlockSignature pm epochSlots = do
    Gen.choice
        [ BlockSignature
              <$> genSignature pm mts
        , BlockPSignatureLight
              <$> genProxySignature pm mts genLightDlgIndices
        , BlockPSignatureHeavy
              <$> genProxySignature pm mts genHeavyDlgIndex
        ]
  where
    mts = genMainToSign pm epochSlots

genGenesisBlockHeader :: ProtocolMagic -> SlotCount -> Gen GenesisBlockHeader
genGenesisBlockHeader pm epochSlots = do
    epoch      <- genEpochIndex
    body       <- genGenesisBody
    prevHeader <- BlockHeaderMain <$> genMainBlockHeader pm epochSlots
    pure $ mkGenesisHeader pm (Right prevHeader) epoch body

genGenesisBody :: Gen GenesisBody
genGenesisBody = GenesisBody <$> genSlotLeaders

genGenesisConsensusData :: Gen GenesisConsensusData
genGenesisConsensusData =
    GenesisConsensusData
        <$> genEpochIndex
        <*> genChainDifficulty

genHeaderHash :: Gen HeaderHash
genHeaderHash = coerce <$> genTextHash

genGenesisProof :: Gen GenesisProof
genGenesisProof = GenesisProof <$> genAbstractHash genSlotLeaders

genMainBody :: ProtocolMagic -> Gen MainBody
genMainBody pm =
    MainBody
        <$> genTxPayload pm
        <*> genSscPayload pm
        <*> genDlgPayload pm
        <*> genUpdatePayload pm

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
genMainBlockHeader :: ProtocolMagic -> SlotCount -> Gen MainBlockHeader
genMainBlockHeader pm epochSlots =
    mkMainHeaderExplicit pm
        <$> genHeaderHash
        <*> genChainDifficulty
        <*> genSlotId epochSlots
        <*> genSecretKey
        <*> pure Nothing
        <*> genMainBody pm
        <*> genMainExtraHeaderData

genMainConsensusData :: ProtocolMagic -> SlotCount -> Gen MainConsensusData
genMainConsensusData pm epochSlots =
    MainConsensusData
        <$> genSlotId epochSlots
        <*> genPublicKey
        <*> genChainDifficulty
        <*> genBlockSignature pm epochSlots


genMainExtraBodyData :: Gen MainExtraBodyData
genMainExtraBodyData = MainExtraBodyData <$> genBlockBodyAttributes

genMainExtraHeaderData :: Gen MainExtraHeaderData
genMainExtraHeaderData =
    MainExtraHeaderData
        <$> genBlockVersion
        <*> genSoftwareVersion
        <*> genBlockHeaderAttributes
        <*> genAbstractHash genMainExtraBodyData

genMainProof :: ProtocolMagic -> Gen MainProof
genMainProof pm =
    MainProof
        <$> genTxProof pm
        <*> genSscProof pm
        <*> genAbstractHash (genDlgPayload pm)
        <*> genUpdateProof pm

genMainToSign :: ProtocolMagic -> SlotCount -> Gen MainToSign
genMainToSign pm epochSlots =
    MainToSign
        <$> genAbstractHash (genBlockHeader pm epochSlots)
        <*> genMainProof pm
        <*> genSlotId epochSlots
        <*> genChainDifficulty
        <*> genMainExtraHeaderData
