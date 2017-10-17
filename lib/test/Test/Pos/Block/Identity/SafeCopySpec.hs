-- | This module tests SafeCopy instances for Block types.

module Test.Pos.Block.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec          (Spec, describe)
import           Universum

import           Pos.Arbitrary.Block ()
import qualified Pos.Block.Core      as BT
import           Pos.Ssc.GodTossing  (SscGodTossing)

import           Test.Pos.Util       (safeCopyTest, withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "Block types" $ do
    describe "SafeCopy instances" $ do
        describe "GenericBlockHeader" $ do
            describe "GenesisBlockHeader" $ do
                safeCopyTest @BT.GenesisBlockHeader
            describe "MainBlockHeader" $ do
                safeCopyTest @BT.MainBlockHeader
        describe "GenesisBlockchain" $ do
            describe "BodyProof" $ do
                safeCopyTest @(BT.BodyProof (BT.GenesisBlockchain SscGodTossing))
            describe "ConsensusData" $ do
                safeCopyTest @(BT.ConsensusData (BT.GenesisBlockchain SscGodTossing))
            describe "Body" $ do
                safeCopyTest @(BT.Body (BT.GenesisBlockchain SscGodTossing))
        describe "MainBlockchain" $ do
            safeCopyTest @BT.MainExtraHeaderData
            safeCopyTest @BT.MainExtraBodyData
            describe "BodyProof" $ do
                safeCopyTest @(BT.BodyProof (BT.MainBlockchain SscGodTossing))
            describe "BlockSignature" $ do
                safeCopyTest @(BT.BlockSignature SscGodTossing)
            describe "ConsensusData" $ do
                safeCopyTest @(BT.ConsensusData (BT.MainBlockchain SscGodTossing))
            describe "Body" $ do
                safeCopyTest @(BT.Body (BT.MainBlockchain SscGodTossing))
