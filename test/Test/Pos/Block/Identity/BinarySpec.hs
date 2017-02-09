{-# LANGUAGE ScopedTypeVariables #-}

-- | This module tests Binary instances for Block types.

module Test.Pos.Block.Identity.BinarySpec
       ( spec
       ) where

import           Data.Reflection            (reify)
import           Serokell.Data.Memory.Units (Byte)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Universum

import           Pos.Block.Arbitrary        ()
import qualified Pos.Block.Network          as BT
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Ssc.NistBeacon         (SscNistBeacon)
import qualified Pos.Types                  as BT

import           Test.Pos.Util              (binaryTest, networkBinaryEncodeDecode,
                                             networkBinaryTest)

spec :: Spec
spec = describe "Block types" $ do
    describe "Bi instances" $ do
        describe "Block network types" $ do
            describe "MsgGetHeaders" $
                networkBinaryTest @BT.MsgGetHeaders
            describe "MsgGetBlocks" $
                networkBinaryTest @BT.MsgGetBlocks
            describe "MsgHeaders" $ do
                networkBinaryTest @(BT.MsgHeaders SscNistBeacon)
                networkBinaryTest @(BT.MsgHeaders SscGodTossing)
            describe "MsgBlock" $ do
              reify (1000000 :: Byte) $ \(_ :: Proxy s0) -> do
                -- We can't use 'networkBinaryTest' here because 's0'
                -- isn't guaranteed to be Typeable.
                prop "MsgBlock SscNistBeacon" $
                    networkBinaryEncodeDecode @(BT.MsgBlock SscNistBeacon)
                prop "MsgBlock SscGodTossing" $
                    networkBinaryEncodeDecode @(BT.MsgBlock SscGodTossing)
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
                    binaryTest @(BT.BodyProof (BT.GenesisBlockchain SscNistBeacon))
                    binaryTest @(BT.BodyProof (BT.GenesisBlockchain SscGodTossing))
                describe "ConsensusData" $ do
                    binaryTest @(BT.ConsensusData (BT.GenesisBlockchain SscNistBeacon))
                    binaryTest @(BT.ConsensusData (BT.GenesisBlockchain SscGodTossing))
                describe "Body" $ do
                    binaryTest @(BT.Body (BT.GenesisBlockchain SscNistBeacon))
                    binaryTest @(BT.Body (BT.GenesisBlockchain SscGodTossing))
            describe "MainBlockchain" $ do
                binaryTest @BT.MainExtraHeaderData
                binaryTest @BT.MainExtraBodyData
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
