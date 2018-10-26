{-# LANGUAGE TemplateHaskell #-}

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
                     MainProof (..), MainToSign (..), SlogUndo (..), Undo (..),
                     mkGenesisHeader, mkMainHeaderExplicit)
import           Pos.Chain.Delegation (DlgPayload (..))
import           Pos.Chain.Genesis (GenesisHash (..))
import           Pos.Core (EpochIndex (..))
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Crypto (Hash, ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..), SignTag (..), abstractHash,
                     createPsk, hash, proxySign, sign, toPublic)

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Chain.Block.Gen
import           Test.Pos.Chain.Delegation.Example (exampleLightDlgIndices,
                     staticHeavyDlgIndexes, staticProxySKHeavys)
import qualified Test.Pos.Chain.Delegation.Example as Delegation
import           Test.Pos.Chain.Ssc.Example (exampleSscPayload, exampleSscProof)
import           Test.Pos.Chain.Txp.Example (exampleTxPayload, exampleTxProof,
                     exampleTxpUndo)
import           Test.Pos.Chain.Update.Example (exampleBlockVersion,
                     exampleSoftwareVersion, exampleUpdatePayload,
                     exampleUpdateProof)
import qualified Test.Pos.Chain.Update.Example as Update
import           Test.Pos.Core.ExampleHelpers (exampleChainDifficulty,
                     exampleEpochIndex, examplePublicKey, exampleSecretKey,
                     exampleSecretKeys, exampleSlotId, exampleSlotLeaders,
                     feedPM, feedPMEpochSlots)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)


--------------------------------------------------------------------------------
-- BlockBodyAttributes
--------------------------------------------------------------------------------

golden_BlockBodyAttributes :: Property
golden_BlockBodyAttributes = goldenTestBi bba "test/golden/bi/block/BlockBodyAttributes"
  where
    bba = mkAttributes ()

roundTripBlockBodyAttributesBi :: Property
roundTripBlockBodyAttributesBi = eachOf 1000 genBlockBodyAttributes roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BlockHeader
--------------------------------------------------------------------------------

golden_BlockHeader_Genesis :: Property
golden_BlockHeader_Genesis =
    goldenTestBi exampleBlockHeaderGenesis "test/golden/bi/block/BlockHeader_Genesis"

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
golden_BlockHeaderMain :: Property
golden_BlockHeaderMain =
    goldenTestBi exampleBlockHeaderMain "test/golden/bi/block/BlockHeaderMain"

roundTripBlockHeaderBi :: Property
roundTripBlockHeaderBi =
    eachOf 10 (feedPMEpochSlots genBlockHeader) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BlockHeaderAttributes
--------------------------------------------------------------------------------

golden_BlockHeaderAttributes :: Property
golden_BlockHeaderAttributes = goldenTestBi (mkAttributes () :: BlockHeaderAttributes)
                                            "test/golden/bi/block/BlockHeaderAttributes"

roundTripBlockHeaderAttributesBi :: Property
roundTripBlockHeaderAttributesBi = eachOf 1000 genBlockHeaderAttributes roundTripsBiBuildable


--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------

golden_BlockSignature :: Property
golden_BlockSignature = goldenTestBi exampleBlockSignature "test/golden/bi/block/BlockSignature"

golden_BlockSignature_Light :: Property
golden_BlockSignature_Light =
    goldenTestBi exampleBlockPSignatureLight "test/golden/bi/block/BlockSignature_Light"

golden_BlockSignature_Heavy :: Property
golden_BlockSignature_Heavy =
    goldenTestBi exampleBlockPSignatureHeavy "test/golden/bi/block/BlockSignature_Heavy"

roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi =
    eachOf 10 (feedPMEpochSlots genBlockSignature) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- GenesisBlockHeader
--------------------------------------------------------------------------------

golden_GenesisBlockHeader :: Property
golden_GenesisBlockHeader = goldenTestBi exampleGenesisBlockHeader
                                         "test/golden/bi/block/GenesisBlockHeader"

roundTripGenesisBlockHeaderBi :: Property
roundTripGenesisBlockHeaderBi =
    eachOf 20 (feedPMEpochSlots genGenesisBlockHeader) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- GenesisBody
--------------------------------------------------------------------------------

golden_GenesisBody :: Property
golden_GenesisBody = goldenTestBi exampleGenesisBody "test/golden/bi/block/GenesisBody"

roundTripGenesisBodyBi :: Property
roundTripGenesisBodyBi = eachOf 1000 genGenesisBody roundTripsBiShow


--------------------------------------------------------------------------------
-- GenesisConsensusData
--------------------------------------------------------------------------------

golden_GenesisConsensusData :: Property
golden_GenesisConsensusData = goldenTestBi cd "test/golden/bi/block/GenesisConsensusData"
  where cd = GenesisConsensusData exampleEpochIndex exampleChainDifficulty

roundTripGenesisConsensusDataBi :: Property
roundTripGenesisConsensusDataBi = eachOf 1000 genGenesisConsensusData roundTripsBiShow


--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------

golden_HeaderHash :: Property
golden_HeaderHash = goldenTestBi exampleHeaderHash "test/golden/bi/block/HeaderHash"

roundTripHeaderHashBi :: Property
roundTripHeaderHashBi = eachOf 1000 genHeaderHash roundTripsBiBuildable


--------------------------------------------------------------------------------
-- GenesisProof
--------------------------------------------------------------------------------

golden_GenesisProof :: Property
golden_GenesisProof = goldenTestBi gp "test/golden/bi/block/GenesisProof"
  where gp = GenesisProof (abstractHash exampleSlotLeaders)

roundTripGenesisProofBi :: Property
roundTripGenesisProofBi = eachOf 1000 genGenesisProof roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainBlockHeader
--------------------------------------------------------------------------------

golden_MainBlockHeader :: Property
golden_MainBlockHeader = goldenTestBi exampleMainBlockHeader "test/golden/bi/block/MainBlockHeader"

roundTripMainBlockHeaderBi :: Property
roundTripMainBlockHeaderBi =
    eachOf 20 (feedPMEpochSlots genMainBlockHeader) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainBody
--------------------------------------------------------------------------------

golden_MainBody :: Property
golden_MainBody = goldenTestBi exampleMainBody "test/golden/bi/block/MainBody"

roundTripMainBodyBi :: Property
roundTripMainBodyBi = eachOf 20 (feedPM genMainBody) roundTripsBiShow


--------------------------------------------------------------------------------
-- MainConsensusData
--------------------------------------------------------------------------------

golden_MainConsensusData :: Property
golden_MainConsensusData = goldenTestBi mcd "test/golden/bi/block/MainConsensusData"
  where mcd = MainConsensusData exampleSlotId examplePublicKey
                                exampleChainDifficulty exampleBlockSignature

roundTripMainConsensusData :: Property
roundTripMainConsensusData =
    eachOf 20 (feedPMEpochSlots genMainConsensusData) roundTripsBiShow


--------------------------------------------------------------------------------
-- MainExtraBodyData
--------------------------------------------------------------------------------

golden_MainExtraBodyData :: Property
golden_MainExtraBodyData = goldenTestBi mebd "test/golden/bi/block/MainExtraBodyData"
  where mebd = MainExtraBodyData (mkAttributes ())

roundTripMainExtraBodyDataBi :: Property
roundTripMainExtraBodyDataBi = eachOf 1000 genMainExtraBodyData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainExtraHeaderData

--------------------------------------------------------------------------------
golden_MainExtraHeaderData :: Property
golden_MainExtraHeaderData = goldenTestBi exampleMainExtraHeaderData
                                          "test/golden/bi/block/MainExtraHeaderData"

roundTripMainExtraHeaderDataBi :: Property
roundTripMainExtraHeaderDataBi = eachOf 1000 genMainExtraHeaderData roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainProof
--------------------------------------------------------------------------------

golden_MainProof :: Property
golden_MainProof = goldenTestBi exampleMainProof "test/golden/bi/block/MainProof"

roundTripMainProofBi :: Property
roundTripMainProofBi = eachOf 20 (feedPM genMainProof) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- MainToSign
--------------------------------------------------------------------------------

golden_MainToSign :: Property
golden_MainToSign = goldenTestBi exampleMainToSign "test/golden/bi/block/MainToSign"

roundTripMainToSignBi :: Property
roundTripMainToSignBi =
    eachOf 20 (feedPMEpochSlots genMainToSign) roundTripsBiShow


--------------------------------------------------------------------------------
-- Undo
--------------------------------------------------------------------------------

golden_Undo :: Property
golden_Undo = goldenTestBi exampleUndo "test/golden/bi/block/Undo"

roundTripUndo :: Property
roundTripUndo = eachOf 20 (feedPMEpochSlots genUndo) roundTripsBiShow


--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleBlockHeaderGenesis :: BlockHeader
exampleBlockHeaderGenesis = (BlockHeaderGenesis exampleGenesisBlockHeader)

exampleBlockHeaderMain :: MainBlockHeader
exampleBlockHeaderMain =
  mkMainHeaderExplicit pm exampleHeaderHash
                       exampleChainDifficulty exampleSlotId
                       exampleSecretKey Nothing
                       exampleMainBody exampleMainExtraHeaderData
  where
    pm = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 0
                       , getRequiresNetworkMagic = RequiresNoMagic
                       }

exampleBlockSignature :: BlockSignature
exampleBlockSignature = BlockSignature (sign pm
                                             SignMainBlock
                                             exampleSecretKey
                                             exampleMainToSign)
  where
    pm = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 7
                       , getRequiresNetworkMagic = RequiresNoMagic
                       }

exampleBlockPSignatureLight :: BlockSignature
exampleBlockPSignatureLight = BlockPSignatureLight sig
  where
    sig = proxySign pm SignProxySK delegateSk psk exampleMainToSign
    [delegateSk, issuerSk] = exampleSecretKeys 5 2
    psk = createPsk pm issuerSk (toPublic delegateSk) exampleLightDlgIndices
    pm = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 2
                       , getRequiresNetworkMagic = RequiresNoMagic
                       }

exampleBlockPSignatureHeavy :: BlockSignature
exampleBlockPSignatureHeavy = BlockPSignatureHeavy sig
  where
    sig = proxySign pm SignProxySK delegateSk psk exampleMainToSign
    [delegateSk, issuerSk] = exampleSecretKeys 5 2
    psk = createPsk pm issuerSk (toPublic delegateSk) (staticHeavyDlgIndexes !! 0)
    pm = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 2
                       , getRequiresNetworkMagic = RequiresNoMagic
                       }

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
exampleGenesisBlockHeader = mkGenesisHeader pm
                                            (Left (GenesisHash prevHash))
                                            (EpochIndex 11)
                                            exampleGenesisBody
  where
    prevHash = coerce (hash ("genesisHash" :: Text)) :: Hash a
    pm = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 0
                       , getRequiresNetworkMagic = RequiresNoMagic
                       }

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
exampleMainBlockHeader :: MainBlockHeader
exampleMainBlockHeader = mkMainHeaderExplicit pm
                                              exampleHeaderHash
                                              exampleChainDifficulty
                                              exampleSlotId
                                              exampleSecretKey
                                              Nothing
                                              exampleMainBody
                                              exampleMainExtraHeaderData
  where
    pm = ProtocolMagic { getProtocolMagicId = ProtocolMagicId 7
                       , getRequiresNetworkMagic = RequiresNoMagic
                       }

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

exampleSlogUndo :: SlogUndo
exampleSlogUndo = SlogUndo $ Just 999

exampleUndo :: Undo
exampleUndo = Undo
  { undoTx = exampleTxpUndo
  , undoDlg = Delegation.exampleUndo
  , undoUS = Update.exampleUndo
  , undoSlog = exampleSlogUndo
  }


--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel $$discoverRoundTrip
    ]
