-- | This module tests SafeCopy instances for Block types.

module Test.Pos.Block.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (prop)
import           Universum

import           Pos.Block.Arbitrary     ()
import           Pos.Ssc.GodTossing      (SscGodTossing)
import           Pos.Ssc.NistBeacon      (SscNistBeacon)
import qualified Pos.Types               as BT

import           Test.Pos.Util           (safeCopyEncodeDecode)

spec :: Spec
spec = describe "Block types" $ do
    describe "SafeCopy instances" $ do
        describe "GenericBlockHeader" $ do
            describe "GenesisBlockHeader" $ do
                prop "NistBeacon"
                    (safeCopyEncodeDecode @(BT.GenesisBlockHeader SscNistBeacon))
                prop "GodTossing"
                    (safeCopyEncodeDecode @(BT.GenesisBlockHeader SscGodTossing))
            describe "MainBlockHeader" $ do
                prop "NistBeacon"
                    (safeCopyEncodeDecode @(BT.MainBlockHeader SscNistBeacon))
                prop "GodTossing"
                    (safeCopyEncodeDecode @(BT.MainBlockHeader SscGodTossing))
        describe "GenesisBlockchain" $ do
            describe "BodyProof" $ do
                prop "NistBeacon"
                    (safeCopyEncodeDecode
                         @(BT.BodyProof (BT.GenesisBlockchain SscNistBeacon)))
                prop "GodTossing"
                    (safeCopyEncodeDecode
                         @(BT.BodyProof (BT.GenesisBlockchain SscGodTossing)))
            describe "ConsensusData" $ do
                prop "NistBeacon"
                    (safeCopyEncodeDecode
                         @(BT.ConsensusData (BT.GenesisBlockchain SscNistBeacon)))
                prop "GodTossing"
                    (safeCopyEncodeDecode
                         @(BT.ConsensusData (BT.GenesisBlockchain SscGodTossing)))
            describe "Body" $ do
                prop "NistBeacon"
                    (safeCopyEncodeDecode @(BT.Body (BT.GenesisBlockchain SscNistBeacon)))
                prop "GodTossing"
                    (safeCopyEncodeDecode @(BT.Body (BT.GenesisBlockchain SscGodTossing)))
        describe "MainBlockchain" $ do
            describe "BodyProof" $ do
                prop "NistBeacon"
                    (safeCopyEncodeDecode
                         @(BT.BodyProof (BT.MainBlockchain SscNistBeacon)))
                prop "GodTossing"
                    (safeCopyEncodeDecode
                         @(BT.BodyProof (BT.MainBlockchain SscGodTossing)))
            describe "BlockSignature" $ do
                prop "NistBeacon"
                    (safeCopyEncodeDecode @(BT.BlockSignature SscNistBeacon))
                prop "GodTossing"
                    (safeCopyEncodeDecode @(BT.BlockSignature SscGodTossing))
            describe "ConsensusData" $ do
                prop "NistBeacon"
                    (safeCopyEncodeDecode
                         @(BT.ConsensusData (BT.MainBlockchain SscNistBeacon)))
                prop "GodTossing"
                    (safeCopyEncodeDecode
                         @(BT.ConsensusData (BT.MainBlockchain SscGodTossing)))
            describe "Body" $ do
                prop "NistBeacon"
                    (safeCopyEncodeDecode @(BT.Body (BT.MainBlockchain SscNistBeacon)))
                prop "GodTossing"
                    (safeCopyEncodeDecode @(BT.Body (BT.MainBlockchain SscGodTossing)))
