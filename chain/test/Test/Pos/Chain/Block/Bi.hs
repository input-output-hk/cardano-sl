module Test.Pos.Chain.Block.Bi
       ( tests
       ) where

import           Universum

import           Data.Coerce (coerce)
import           Data.List ((!!))
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Chain.Block (BlockHeader (..), BlockHeaderAttributes,
                     BlockSignature (..), GenesisBlockHeader, GenesisBody (..),
                     GenesisConsensusData (..), GenesisProof (..), HeaderHash,
                     MainBlockHeader, MainBody (..), MainConsensusData (..),
                     MainExtraBodyData (..), MainExtraHeaderData (..),
                     MainProof (..), MainToSign (..), mkGenesisHeader,
                     mkMainHeaderExplicit)
import           Pos.Core (EpochIndex (..), ProtocolMagic (..))
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Core.Configuration (GenesisHash (..))
import           Pos.Core.Delegation (DlgPayload (..))
import           Pos.Crypto (Hash, SignTag (..), abstractHash, createPsk, hash,
                     proxySign, sign, toPublic)

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Chain.Block.Gen
import           Test.Pos.Core.Bi (exampleBlockVersion, exampleChainDifficulty,
                     exampleEpochIndex, exampleLightDlgIndices,
                     examplePublicKey, exampleSecretKey, exampleSecretKeys,
                     exampleSlotId, exampleSlotLeaders, exampleSoftwareVersion,
                     exampleSscPayload, exampleSscProof, exampleTxPayload,
                     exampleTxProof, exampleUpdatePayload, exampleUpdateProof,
                     feedPM, feedPMC, staticHeavyDlgIndexes,
                     staticProxySKHeavys)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)

--------------------------------------------------------------------------------
-- BlockBodyAttributes
--------------------------------------------------------------------------------
golden_BlockBodyAttributes :: Property
golden_BlockBodyAttributes = goldenTestBi bba "test/golden/BlockBodyAttributes"
  where
    bba = mkAttributes ()

roundTripBlockBodyAttributesBi :: Property
roundTripBlockBodyAttributesBi = eachOf 1000 genBlockBodyAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockHeader
--------------------------------------------------------------------------------
golden_BlockHeader_Genesis :: Property
golden_BlockHeader_Genesis =
    goldenTestBi exampleBlockHeaderGenesis "test/golden/BlockHeader_Genesis"

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
golden_BlockHeaderMain :: Property
golden_BlockHeaderMain =
    goldenTestBi exampleBlockHeaderMain "test/golden/BlockHeaderMain"

roundTripBlockHeaderBi :: Property
roundTripBlockHeaderBi = eachOf 10 (feedPMC genBlockHeader) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockHeaderAttributes
--------------------------------------------------------------------------------
golden_BlockHeaderAttributes :: Property
golden_BlockHeaderAttributes = goldenTestBi (mkAttributes () :: BlockHeaderAttributes)
                                            "test/golden/BlockHeaderAttributes"

roundTripBlockHeaderAttributesBi :: Property
roundTripBlockHeaderAttributesBi = eachOf 1000 genBlockHeaderAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------
golden_BlockSignature :: Property
golden_BlockSignature = goldenTestBi exampleBlockSignature "test/golden/BlockSignature"

golden_BlockSignature_Light :: Property
golden_BlockSignature_Light =
    goldenTestBi exampleBlockPSignatureLight "test/golden/BlockSignature_Light"

golden_BlockSignature_Heavy :: Property
golden_BlockSignature_Heavy =
    goldenTestBi exampleBlockPSignatureHeavy "test/golden/BlockSignature_Heavy"

roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi = eachOf 10 (feedPMC genBlockSignature) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- GenesisBlockHeader
--------------------------------------------------------------------------------
golden_GenesisBlockHeader :: Property
golden_GenesisBlockHeader = goldenTestBi exampleGenesisBlockHeader
                                         "test/golden/GenesisBlockHeader"

roundTripGenesisBlockHeaderBi :: Property
roundTripGenesisBlockHeaderBi = eachOf 1000 (feedPM genGenesisBlockHeader) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- GenesisBody
--------------------------------------------------------------------------------
golden_GenesisBody :: Property
golden_GenesisBody = goldenTestBi exampleGenesisBody "test/golden/GenesisBody"

roundTripGenesisBodyBi :: Property
roundTripGenesisBodyBi = eachOf 1000 genGenesisBody roundTripsBiShow

--------------------------------------------------------------------------------
-- GenesisConsensusData
--------------------------------------------------------------------------------
golden_GenesisConsensusData :: Property
golden_GenesisConsensusData = goldenTestBi cd "test/golden/GenesisConsensusData"
  where cd = GenesisConsensusData exampleEpochIndex exampleChainDifficulty

roundTripGenesisConsensusDataBi :: Property
roundTripGenesisConsensusDataBi = eachOf 1000 genGenesisConsensusData roundTripsBiShow

--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------
golden_HeaderHash :: Property
golden_HeaderHash = goldenTestBi exampleHeaderHash "test/golden/HeaderHash"

roundTripHeaderHashBi :: Property
roundTripHeaderHashBi = eachOf 1000 genHeaderHash roundTripsBiBuildable

--------------------------------------------------------------------------------
-- GenesisProof
--------------------------------------------------------------------------------
golden_GenesisProof :: Property
golden_GenesisProof = goldenTestBi gp "test/golden/GenesisProof"
  where gp = GenesisProof (abstractHash exampleSlotLeaders)

roundTripGenesisProofBi :: Property
roundTripGenesisProofBi = eachOf 1000 genGenesisProof roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainBlockHeader
--------------------------------------------------------------------------------
golden_MainBlockHeader :: Property
golden_MainBlockHeader = goldenTestBi exampleMainBlockHeader "test/golden/MainBlockHeader"

roundTripMainBlockHeaderBi :: Property
roundTripMainBlockHeaderBi = eachOf 20 (feedPMC genMainBlockHeader) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainBody
--------------------------------------------------------------------------------
golden_MainBody :: Property
golden_MainBody = goldenTestBi exampleMainBody "test/golden/MainBody"

roundTripMainBodyBi :: Property
roundTripMainBodyBi = eachOf 20 (feedPM genMainBody) roundTripsBiShow

--------------------------------------------------------------------------------
-- MainConsensusData
--------------------------------------------------------------------------------
golden_MainConsensusData :: Property
golden_MainConsensusData = goldenTestBi mcd "test/golden/MainConsensusData"
  where mcd = MainConsensusData exampleSlotId examplePublicKey
                                exampleChainDifficulty exampleBlockSignature

roundTripMainConsensusData :: Property
roundTripMainConsensusData = eachOf 20 (feedPMC genMainConsensusData) roundTripsBiShow

--------------------------------------------------------------------------------
-- MainExtraBodyData
--------------------------------------------------------------------------------
golden_MainExtraBodyData :: Property
golden_MainExtraBodyData = goldenTestBi mebd "test/golden/MainExtraBodyData"
  where mebd = MainExtraBodyData (mkAttributes ())

roundTripMainExtraBodyDataBi :: Property
roundTripMainExtraBodyDataBi = eachOf 1000 genMainExtraBodyData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainExtraHeaderData
--------------------------------------------------------------------------------
golden_MainExtraHeaderData :: Property
golden_MainExtraHeaderData = goldenTestBi exampleMainExtraHeaderData
                                          "test/golden/MainExtraHeaderData"

roundTripMainExtraHeaderDataBi :: Property
roundTripMainExtraHeaderDataBi = eachOf 1000 genMainExtraHeaderData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainProof
--------------------------------------------------------------------------------
golden_MainProof :: Property
golden_MainProof = goldenTestBi exampleMainProof "test/golden/MainProof"

roundTripMainProofBi :: Property
roundTripMainProofBi = eachOf 20 (feedPM genMainProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainToSign
--------------------------------------------------------------------------------
golden_MainToSign :: Property
golden_MainToSign = goldenTestBi exampleMainToSign "test/golden/MainToSign"

roundTripMainToSignBi :: Property
roundTripMainToSignBi = eachOf 20 (feedPMC genMainToSign) roundTripsBiShow

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleBlockHeaderGenesis :: BlockHeader
exampleBlockHeaderGenesis = (BlockHeaderGenesis exampleGenesisBlockHeader)

exampleBlockHeaderMain :: MainBlockHeader
exampleBlockHeaderMain =
  mkMainHeaderExplicit (ProtocolMagic 0) exampleHeaderHash
                       exampleChainDifficulty exampleSlotId
                       exampleSecretKey Nothing
                       exampleMainBody exampleMainExtraHeaderData

exampleBlockSignature :: BlockSignature
exampleBlockSignature = BlockSignature (sign (ProtocolMagic 7)
                                              SignMainBlock
                                              exampleSecretKey
                                              exampleMainToSign)

exampleBlockPSignatureLight :: BlockSignature
exampleBlockPSignatureLight = BlockPSignatureLight sig
  where
    sig = proxySign pm SignProxySK delegateSk psk exampleMainToSign
    [delegateSk, issuerSk] = exampleSecretKeys 5 2
    psk = createPsk pm issuerSk (toPublic delegateSk) exampleLightDlgIndices
    pm = ProtocolMagic 2

exampleBlockPSignatureHeavy :: BlockSignature
exampleBlockPSignatureHeavy = BlockPSignatureHeavy sig
  where
    sig = proxySign pm SignProxySK delegateSk psk exampleMainToSign
    [delegateSk, issuerSk] = exampleSecretKeys 5 2
    psk = createPsk pm issuerSk (toPublic delegateSk) (staticHeavyDlgIndexes !! 0)
    pm = ProtocolMagic 2

exampleMainConsensusData :: MainConsensusData
exampleMainConsensusData = MainConsensusData exampleSlotId
                                             examplePublicKey
                                             exampleChainDifficulty
                                             exampleBlockSignature

exampleMainExtraHeaderData :: MainExtraHeaderData
exampleMainExtraHeaderData =
    MainExtraHeaderData exampleBlockVersion
                        exampleSoftwareVersion
                        (mkAttributes ())
                        (abstractHash (MainExtraBodyData (mkAttributes ())))

exampleGenesisBlockHeader :: GenesisBlockHeader
exampleGenesisBlockHeader = mkGenesisHeader (ProtocolMagic 0)
                                            (Left (GenesisHash prevHash))
                                            (EpochIndex 11)
                                            exampleGenesisBody
  where
    prevHash = coerce (hash ("genesisHash" :: Text)) :: Hash a

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
exampleMainBlockHeader :: MainBlockHeader
exampleMainBlockHeader = mkMainHeaderExplicit (ProtocolMagic 7)
                                              exampleHeaderHash
                                              exampleChainDifficulty
                                              exampleSlotId
                                              exampleSecretKey
                                              Nothing
                                              exampleMainBody
                                              exampleMainExtraHeaderData

exampleMainProof :: MainProof
exampleMainProof = MainProof exampleTxProof exampleSscProof
                             (abstractHash dp) exampleUpdateProof
  where
    dp = UnsafeDlgPayload (take 4 staticProxySKHeavys)

exampleHeaderHash :: HeaderHash
exampleHeaderHash = coerce (hash ("HeaderHash" :: Text))

exampleGenesisBody :: GenesisBody
exampleGenesisBody = GenesisBody exampleSlotLeaders

exampleMainBody :: MainBody
exampleMainBody = MainBody exampleTxPayload exampleSscPayload
                           dp exampleUpdatePayload
  where
    dp = UnsafeDlgPayload (take 4 staticProxySKHeavys)

exampleMainToSign :: MainToSign
exampleMainToSign = MainToSign (abstractHash (BlockHeaderGenesis exampleGenesisBlockHeader))
                    exampleMainProof exampleSlotId exampleChainDifficulty exampleMainExtraHeaderData

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel $$discoverRoundTrip
    ]
