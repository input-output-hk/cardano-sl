-- | This module tests Binary instances for Block types.

module Test.Pos.Block.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (prop)
import           Universum

import           Pos.Block.Arbitrary     ()
import qualified Pos.Block.Network       as BT
import           Pos.Ssc.GodTossing      (SscGodTossing)
import           Pos.Ssc.NistBeacon      (SscNistBeacon)
import qualified Pos.Types               as BT

import           Test.Pos.Util           (binaryEncodeDecode)

spec :: Spec
spec = describe "Block types" $ do
    describe "Bi instances" $ do
        describe "Block network types" $ do
            describe "MsgGetHeaders" $ do
                prop "NistBeacon" (binaryEncodeDecode @(BT.MsgGetHeaders SscNistBeacon))
                prop "GodTossing" (binaryEncodeDecode @(BT.MsgGetHeaders SscGodTossing))
            describe "MsgGetBlocks" $ do
                prop "NistBeacon" (binaryEncodeDecode @(BT.MsgGetBlocks SscNistBeacon))
                prop "GodTossing" (binaryEncodeDecode @(BT.MsgGetBlocks SscGodTossing))
            describe "MsgHeaders" $ do
                prop "NistBeacon" (binaryEncodeDecode @(BT.MsgHeaders SscNistBeacon))
                prop "GodTossing" (binaryEncodeDecode @(BT.MsgHeaders SscGodTossing))
            describe "MsgBlock" $ do
                prop "NistBeacon" (binaryEncodeDecode @(BT.MsgBlock SscNistBeacon))
                prop "GodTossing" (binaryEncodeDecode @(BT.MsgBlock SscGodTossing))
        describe "Blockchains and blockheaders" $ do
            describe "GenericBlockHeader" $ do
                describe "GenesisBlockHeader" $ do
                    prop "NistBeacon"
                        (binaryEncodeDecode @(BT.GenesisBlockHeader SscNistBeacon))
                    prop "GodTossing"
                        (binaryEncodeDecode @(BT.GenesisBlockHeader SscGodTossing))
                describe "MainBlockHeader" $ do
                    prop "NistBeacon" (binaryEncodeDecode
                                           @(BT.MainBlockHeader SscNistBeacon))
                    prop "GodTossing" (binaryEncodeDecode
                                           @(BT.MainBlockHeader SscGodTossing))
            describe "GenesisBlockchain" $ do
                describe "BodyProof" $ do
                    prop "NistBeacon"
                        (binaryEncodeDecode
                             @(BT.BodyProof (BT.GenesisBlockchain SscNistBeacon)))
                    prop "GodTossing"
                        (binaryEncodeDecode
                             @(BT.BodyProof (BT.GenesisBlockchain SscGodTossing)))
                describe "ConsensusData" $ do
                    prop "NistBeacon"
                        (binaryEncodeDecode
                             @(BT.ConsensusData (BT.GenesisBlockchain SscNistBeacon)))
                    prop "GodTossing"
                        (binaryEncodeDecode
                             @(BT.ConsensusData (BT.GenesisBlockchain SscGodTossing)))
                describe "Body" $ do
                    prop "NistBeacon"
                        (binaryEncodeDecode
                             @(BT.Body (BT.GenesisBlockchain SscNistBeacon)))
                    prop "GodTossing"
                        (binaryEncodeDecode
                             @(BT.Body (BT.GenesisBlockchain SscGodTossing)))
            describe "MainBlockchain" $ do
                prop "MainExtraHeaderData" (binaryEncodeDecode @BT.MainExtraHeaderData)
                prop "MainExtraBodyData" (binaryEncodeDecode @BT.MainExtraBodyData)
                describe "BodyProof" $ do
                    prop "NistBeacon"
                        (binaryEncodeDecode
                             @(BT.BodyProof (BT.MainBlockchain SscNistBeacon)))
                    prop "GodTossing"
                        (binaryEncodeDecode
                             @(BT.BodyProof (BT.MainBlockchain SscGodTossing)))
                describe "BlockSignature" $ do
                    prop "NistBeacon" (binaryEncodeDecode
                                           @(BT.BlockSignature SscNistBeacon))
                    prop "GodTossing" (binaryEncodeDecode
                                           @(BT.BlockSignature SscGodTossing))
                describe "ConsensusData" $ do
                    prop "NistBeacon"
                        (binaryEncodeDecode
                             @(BT.ConsensusData (BT.MainBlockchain SscNistBeacon)))
                    prop "GodTossing"
                        (binaryEncodeDecode
                             @(BT.ConsensusData (BT.MainBlockchain SscGodTossing)))
                describe "Body" $ do
                    prop "NistBeacon"
                        (binaryEncodeDecode @(BT.Body (BT.MainBlockchain SscNistBeacon)))
                    prop "GodTossing"
                        (binaryEncodeDecode @(BT.Body (BT.MainBlockchain SscGodTossing)))
