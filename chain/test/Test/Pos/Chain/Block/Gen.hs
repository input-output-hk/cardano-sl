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
                     GenesisConsensusData (..), GenesisExtraHeaderData (..),
                     GenesisProof (..), HeaderHash, MainBlockHeader,
                     MainBody (..), MainConsensusData (..),
                     MainExtraBodyData (..), MainExtraHeaderData (..),
                     MainProof (..), MainToSign (..), mkGenericHeader,
                     mkMainHeaderExplicit)
import           Pos.Core (ProtocolConstants, ProtocolMagic)
import           Pos.Core.Attributes (mkAttributes)

import           Test.Pos.Core.Gen (genBlockVersion, genChainDifficulty,
                     genDlgPayload, genEpochIndex, genHeavyDlgIndex,
                     genLightDlgIndices, genSlotId, genSlotLeaders,
                     genSoftwareVersion, genSscPayload, genSscProof,
                     genTextHash, genTxPayload, genTxProof, genUpdatePayload,
                     genUpdateProof)
import           Test.Pos.Crypto.Gen (genAbstractHash, genProxySignature,
                     genPublicKey, genSecretKey, genSignature)

genBlockBodyAttributes :: Gen BlockBodyAttributes
genBlockBodyAttributes = pure $ mkAttributes ()

genBlockHeader :: ProtocolMagic -> ProtocolConstants -> Gen BlockHeader
genBlockHeader pm pc =
    Gen.choice [ BlockHeaderGenesis <$> genGenesisBlockHeader pm
               , BlockHeaderMain <$> genMainBlockHeader pm pc
               ]

genBlockHeaderAttributes :: Gen BlockHeaderAttributes
genBlockHeaderAttributes = pure $ mkAttributes ()

genBlockSignature :: ProtocolMagic -> ProtocolConstants -> Gen BlockSignature
genBlockSignature pm pc = do
    Gen.choice
        [ BlockSignature
              <$> genSignature pm mts
        , BlockPSignatureLight
              <$> genProxySignature pm mts genLightDlgIndices
        , BlockPSignatureHeavy
              <$> genProxySignature pm mts genHeavyDlgIndex
        ]
  where
    mts = genMainToSign pm pc

genGenesisBlockHeader :: ProtocolMagic -> Gen GenesisBlockHeader
genGenesisBlockHeader pm = do
    epoch      <- genEpochIndex
    body       <- genGenesisBody
    prevHash   <- coerce <$> genTextHash
    difficulty <- genChainDifficulty
    let consensus = const (GenesisConsensusData {_gcdEpoch      = epoch
                                                ,_gcdDifficulty = difficulty})
        gehd      = GenesisExtraHeaderData $ mkAttributes ()
    pure (mkGenericHeader pm prevHash body consensus gehd)

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
genMainBlockHeader :: ProtocolMagic -> ProtocolConstants -> Gen MainBlockHeader
genMainBlockHeader pm pc =
    mkMainHeaderExplicit pm
        <$> genHeaderHash
        <*> genChainDifficulty
        <*> genSlotId pc
        <*> genSecretKey
        <*> pure Nothing
        <*> genMainBody pm
        <*> genMainExtraHeaderData

genMainConsensusData :: ProtocolMagic -> ProtocolConstants -> Gen MainConsensusData
genMainConsensusData pm pc =
    MainConsensusData
        <$> genSlotId pc
        <*> genPublicKey
        <*> genChainDifficulty
        <*> genBlockSignature pm pc


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

genMainToSign :: ProtocolMagic -> ProtocolConstants -> Gen MainToSign
genMainToSign pm pc =
    MainToSign
        <$> genAbstractHash (genBlockHeader pm pc)
        <*> genMainProof pm
        <*> genSlotId pc
        <*> genChainDifficulty
        <*> genMainExtraHeaderData
