module Test.Pos.Chain.Block.Gen
       ( genBlockConfiguration
       , genBlockBodyAttributes
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
       , genUndo
       ) where

import           Universum

import           Data.Coerce (coerce)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Chain.Block (BlockBodyAttributes, BlockConfiguration (..),
                     BlockHeader (..), BlockHeaderAttributes,
                     BlockSignature (..), GenesisBlockHeader, GenesisBody (..),
                     GenesisConsensusData (..), GenesisProof (..), HeaderHash,
                     MainBlockHeader, MainBody (..), MainConsensusData (..),
                     MainExtraBodyData (..), MainExtraHeaderData (..),
                     MainProof (..), MainToSign (..), SlogUndo (..), Undo (..),
                     mkGenesisHeader, mkMainHeaderExplicit)
import           Pos.Core (SlotCount)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Crypto (ProtocolMagic)

import qualified Test.Pos.Chain.Delegation.Gen as Delegation
import           Test.Pos.Chain.Ssc.Gen (genSscPayload, genSscProof)
import           Test.Pos.Chain.Txp.Gen (genTxPayload, genTxProof, genTxpUndo)
import qualified Test.Pos.Chain.Update.Gen as Update
import           Test.Pos.Core.Gen (genChainDifficulty, genEpochIndex,
                     genFlatSlotId, genSlotId, genSlotLeaders, genTextHash)
import           Test.Pos.Crypto.Gen (genAbstractHash, genProxySignature,
                     genPublicKey, genSecretKey, genSignature)


genBlockConfiguration :: Gen BlockConfiguration
genBlockConfiguration = BlockConfiguration <$> Gen.int Range.constantBounded
                                           <*> Gen.int Range.constantBounded
                                           <*> Gen.int Range.constantBounded
                                           <*> Gen.double (Range.constant 0 1)
                                           <*> Gen.double (Range.constant 0 1)
                                           <*> Gen.double (Range.constant 0 1)
                                           <*> Gen.double (Range.constant 0 1)
                                           <*> Gen.int Range.constantBounded
                                           <*> (fromIntegral <$> Gen.int Range.constantBounded)

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
              <$> genProxySignature pm mts Delegation.genLightDlgIndices
        , BlockPSignatureHeavy
              <$> genProxySignature pm mts Delegation.genHeavyDlgIndex
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
        <*> Delegation.genDlgPayload pm
        <*> Update.genUpdatePayload pm

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
        <$> Update.genBlockVersion
        <*> Update.genSoftwareVersion
        <*> genBlockHeaderAttributes
        <*> genAbstractHash genMainExtraBodyData

genMainProof :: ProtocolMagic -> Gen MainProof
genMainProof pm =
    MainProof
        <$> genTxProof pm
        <*> genSscProof pm
        <*> genAbstractHash (Delegation.genDlgPayload pm)
        <*> Update.genUpdateProof pm

genMainToSign :: ProtocolMagic -> SlotCount -> Gen MainToSign
genMainToSign pm epochSlots =
    MainToSign
        <$> genAbstractHash (genBlockHeader pm epochSlots)
        <*> genMainProof pm
        <*> genSlotId epochSlots
        <*> genChainDifficulty
        <*> genMainExtraHeaderData

genSlogUndo :: Gen SlogUndo
genSlogUndo = SlogUndo <$> Gen.maybe genFlatSlotId

genUndo :: ProtocolMagic -> SlotCount -> Gen Undo
genUndo pm epochSlots = Undo
    <$> genTxpUndo
    <*> Delegation.genUndo pm
    <*> Update.genUndo pm epochSlots
    <*> genSlogUndo
