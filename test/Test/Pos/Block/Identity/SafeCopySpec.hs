-- | This module tests SafeCopy instances for Block types.

module Test.Pos.Block.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec          (Spec, describe)
import           Universum

import           Pos.Block.Arbitrary ()
import qualified Pos.Block.Core      as BT
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Ssc.NistBeacon  (SscNistBeacon)

import           Test.Pos.Util       (safeCopyTest)

spec :: Spec
spec = describe "Block types" $ do
    describe "SafeCopy instances" $ do
        describe "GenericBlockHeader" $ do
            describe "GenesisBlockHeader" $ do
                safeCopyTest @(BT.GenesisBlockHeader SscNistBeacon)
                safeCopyTest @(BT.GenesisBlockHeader SscGodTossing)
            describe "MainBlockHeader" $ do
                safeCopyTest @(BT.MainBlockHeader SscNistBeacon)
                safeCopyTest @(BT.MainBlockHeader SscGodTossing)
        describe "GenesisBlockchain" $ do
            describe "BodyProof" $ do
                safeCopyTest @(BT.BodyProof (BT.GenesisBlockchain SscNistBeacon))
                safeCopyTest @(BT.BodyProof (BT.GenesisBlockchain SscGodTossing))
            describe "ConsensusData" $ do
                safeCopyTest @(BT.ConsensusData (BT.GenesisBlockchain SscNistBeacon))
                safeCopyTest @(BT.ConsensusData (BT.GenesisBlockchain SscGodTossing))
            describe "Body" $ do
                safeCopyTest @(BT.Body (BT.GenesisBlockchain SscNistBeacon))
                safeCopyTest @(BT.Body (BT.GenesisBlockchain SscGodTossing))
        describe "MainBlockchain" $ do
            safeCopyTest @BT.MainExtraHeaderData
            safeCopyTest @BT.MainExtraBodyData
            describe "BodyProof" $ do
                safeCopyTest @(BT.BodyProof (BT.MainBlockchain SscNistBeacon))
                safeCopyTest @(BT.BodyProof (BT.MainBlockchain SscGodTossing))
            describe "BlockSignature" $ do
                safeCopyTest @(BT.BlockSignature SscNistBeacon)
                safeCopyTest @(BT.BlockSignature SscGodTossing)
            describe "ConsensusData" $ do
                safeCopyTest @(BT.ConsensusData (BT.MainBlockchain SscNistBeacon))
                safeCopyTest @(BT.ConsensusData (BT.MainBlockchain SscGodTossing))
            describe "Body" $ do
                safeCopyTest @(BT.Body (BT.MainBlockchain SscNistBeacon))
                safeCopyTest @(BT.Body (BT.MainBlockchain SscGodTossing))
