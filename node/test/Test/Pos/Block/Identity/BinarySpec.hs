-- | This module tests Binary instances for Block types.

module Test.Pos.Block.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec          (Spec, describe)

import           Pos.Arbitrary.Block ()
import qualified Pos.Block.Core      as BT
import qualified Pos.Block.Network   as BT
import qualified Pos.Communication   ()
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Ssc.NistBeacon  (SscNistBeacon)

import           Test.Pos.Util       (binaryTest, giveCoreConf)

spec :: Spec
spec = giveCoreConf $ describe "Block types" $ do
    describe "Bi instances" $ do
        describe "Block network types" $ do
            describe "MsgGetHeaders" $
                binaryTest @BT.MsgGetHeaders
            describe "MsgGetBlocks" $
                binaryTest @BT.MsgGetBlocks
            describe "MsgHeaders" $ do
                binaryTest @(BT.MsgHeaders SscNistBeacon)
                binaryTest @(BT.MsgHeaders SscGodTossing)
            describe "MsgBlock" $ do
                binaryTest @(BT.MsgBlock SscNistBeacon)
                binaryTest @(BT.MsgBlock SscGodTossing)
        describe "Blockchains and blockheaders" $ do
            describe "GenericBlockHeader" $ do
                describe "GenesisBlockHeader" $ do
                    binaryTest @(BT.GenesisBlockHeader SscNistBeacon)
                    binaryTest @(BT.GenesisBlockHeader SscGodTossing)
                describe "MainBlockHeader" $ do
                    binaryTest @(BT.MainBlockHeader SscNistBeacon)
                    binaryTest @(BT.MainBlockHeader SscGodTossing)
            describe "GenesisBlockchain" $ do
                describe "BodyProof" $ do
                    binaryTest @BT.GenesisExtraHeaderData
                    binaryTest @BT.GenesisExtraBodyData
                    binaryTest @(BT.BodyProof (BT.GenesisBlockchain SscNistBeacon))
                    binaryTest @(BT.BodyProof (BT.GenesisBlockchain SscGodTossing))
                describe "ConsensusData" $ do
                    binaryTest @(BT.ConsensusData (BT.GenesisBlockchain SscNistBeacon))
                    binaryTest @(BT.ConsensusData (BT.GenesisBlockchain SscGodTossing))
                describe "Body" $ do
                    binaryTest @(BT.Body (BT.GenesisBlockchain SscNistBeacon))
                    binaryTest @(BT.Body (BT.GenesisBlockchain SscGodTossing))
            describe "MainBlockchain" $ do
                describe "BodyProof" $ do
                    binaryTest @(BT.BodyProof (BT.MainBlockchain SscNistBeacon))
                    binaryTest @(BT.BodyProof (BT.MainBlockchain SscGodTossing))
                describe "BlockSignature" $ do
                    binaryTest @(BT.BlockSignature SscNistBeacon)
                    binaryTest @(BT.BlockSignature SscGodTossing)
                describe "ConsensusData" $ do
                    binaryTest @(BT.ConsensusData (BT.MainBlockchain SscNistBeacon))
                    binaryTest @(BT.ConsensusData (BT.MainBlockchain SscGodTossing))
                describe "Body" $ do
                    binaryTest @(BT.Body (BT.MainBlockchain SscNistBeacon))
                    binaryTest @(BT.Body (BT.MainBlockchain SscGodTossing))
                describe "MainToSign" $ do
                    binaryTest @(BT.MainToSign SscNistBeacon)
                    binaryTest @(BT.MainToSign SscGodTossing)
                describe "Extra data" $ do
                    binaryTest @BT.MainExtraHeaderData
                    binaryTest @BT.MainExtraBodyData
