-- | This module tests SafeCopy instances for Block types.

module Test.Pos.Block.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import           Pos.Arbitrary.Block ()
import qualified Pos.Core.Block as BT
import           Pos.SafeCopy ()

import           Test.Pos.Helpers (safeCopyTest)
import           Test.Pos.Util (withDefConfiguration)

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
                safeCopyTest @(BT.BodyProof BT.GenesisBlockchain)
            describe "ConsensusData" $ do
                safeCopyTest @(BT.ConsensusData BT.GenesisBlockchain)
            describe "Body" $ do
                safeCopyTest @(BT.Body BT.GenesisBlockchain)
        describe "MainBlockchain" $ do
            safeCopyTest @BT.MainExtraHeaderData
            safeCopyTest @BT.MainExtraBodyData
            describe "BodyProof" $ do
                safeCopyTest @(BT.BodyProof BT.MainBlockchain)
            describe "BlockSignature" $ do
                safeCopyTest @BT.BlockSignature
            describe "ConsensusData" $ do
                safeCopyTest @(BT.ConsensusData BT.MainBlockchain)
            describe "Body" $ do
                safeCopyTest @(BT.Body BT.MainBlockchain)
