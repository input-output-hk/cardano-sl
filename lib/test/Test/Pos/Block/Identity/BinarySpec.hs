-- | This module tests Binary instances for Block types.

module Test.Pos.Block.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Pos.Arbitrary.Block ()
import qualified Pos.Block.Network as BT
import qualified Pos.Communication ()
import qualified Pos.Core.Block as BT

import           Test.Pos.Helpers (binaryTest)
import           Test.Pos.Util (withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "Block types" $ do
    describe "Bi instances" $ do
        describe "Block network types" $ do
            describe "MsgGetHeaders" $
                binaryTest @BT.MsgGetHeaders
            describe "MsgGetBlocks" $
                binaryTest @BT.MsgGetBlocks
            describe "MsgHeaders" $ do
                binaryTest @BT.MsgHeaders
            describe "MsgBlock" $ do
                binaryTest @BT.MsgBlock
        describe "Blockchains and blockheaders" $ do
            describe "GenericBlockHeader" $ do
                describe "GenesisBlockHeader" $ do
                    binaryTest @BT.GenesisBlockHeader
                describe "MainBlockHeader" $ do
                    binaryTest @BT.MainBlockHeader
            describe "GenesisBlockchain" $ do
                describe "BodyProof" $ do
                    binaryTest @BT.GenesisExtraHeaderData
                    binaryTest @BT.GenesisExtraBodyData
                    binaryTest @(BT.BodyProof BT.GenesisBlockchain)
                describe "ConsensusData" $ do
                    binaryTest @(BT.ConsensusData BT.GenesisBlockchain)
                describe "Body" $ do
                    binaryTest @(BT.Body BT.GenesisBlockchain)
            describe "MainBlockchain" $ do
                describe "BodyProof" $ do
                    binaryTest @(BT.BodyProof BT.MainBlockchain)
                describe "BlockSignature" $ do
                    binaryTest @BT.BlockSignature
                describe "ConsensusData" $ do
                    binaryTest @(BT.ConsensusData BT.MainBlockchain)
                describe "Body" $ do
                    binaryTest @(BT.Body BT.MainBlockchain)
                describe "MainToSign" $ do
                    binaryTest @BT.MainToSign
                describe "Extra data" $ do
                    binaryTest @BT.MainExtraHeaderData
                    binaryTest @BT.MainExtraBodyData
