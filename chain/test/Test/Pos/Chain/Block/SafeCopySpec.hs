{-# LANGUAGE TypeApplications #-}

-- | This module tests SafeCopy instances for Block types.

module Test.Pos.Chain.Block.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import qualified Pos.Chain.Block as Block

import           Test.Pos.Binary.Helpers (safeCopyTest)
import           Test.Pos.Chain.Block.Arbitrary ()

spec :: Spec
spec = describe "Block types" $ do
    describe "SafeCopy instances" $ do
        describe "GenericBlockHeader" $ do
            describe "GenesisBlockHeader" $ do
                safeCopyTest @Block.GenesisBlockHeader
            describe "MainBlockHeader" $ do
                safeCopyTest @Block.MainBlockHeader
        describe "GenesisBlock" $ do
            describe "Proof" $ do
                safeCopyTest @Block.GenesisProof
            describe "ConsensusData" $ do
                safeCopyTest @Block.GenesisConsensusData
            describe "Body" $ do
                safeCopyTest @Block.GenesisBody
        describe "MainBlock" $ do
            safeCopyTest @Block.MainExtraHeaderData
            safeCopyTest @Block.MainExtraBodyData
            describe "Proof" $ do
                safeCopyTest @Block.MainProof
            describe "BlockSignature" $ do
                safeCopyTest @Block.BlockSignature
            describe "ConsensusData" $ do
                safeCopyTest @Block.MainConsensusData
            describe "Body" $ do
                safeCopyTest @Block.MainBody
