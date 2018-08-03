{-# LANGUAGE TypeApplications #-}

-- | This module tests SafeCopy instances for Block types.

module Test.Pos.Chain.Block.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import qualified Pos.Chain.Block as Core
import           Pos.Core.Configuration (defaultCoreConfiguration,
                     withGenesisSpec)

import           Test.Pos.Binary.Helpers (safeCopyTest)
import           Test.Pos.Chain.Block.Arbitrary ()

spec :: Spec
spec = withGenesisSpec 0 defaultCoreConfiguration id $ \_ -> describe "Block types" $ do
    -- These types are defined in 'core' but the 'Arbitrary' instances require
    -- generator components defined in package like 'ssc' and 'update' which
    -- means these tests cannot be moved to 'core'.
    describe "SafeCopy instances" $ do
        describe "GenericBlockHeader" $ do
            describe "GenesisBlockHeader" $ do
                safeCopyTest @Core.GenesisBlockHeader
            describe "MainBlockHeader" $ do
                safeCopyTest @Core.MainBlockHeader
        describe "GenesisBlockchain" $ do
            describe "BodyProof" $ do
                safeCopyTest @(Core.BodyProof Core.GenesisBlockchain)
            describe "ConsensusData" $ do
                safeCopyTest @(Core.ConsensusData Core.GenesisBlockchain)
            describe "Body" $ do
                safeCopyTest @(Core.Body Core.GenesisBlockchain)
        describe "MainBlockchain" $ do
            safeCopyTest @Core.MainExtraHeaderData
            safeCopyTest @Core.MainExtraBodyData
            describe "BodyProof" $ do
                safeCopyTest @(Core.BodyProof Core.MainBlockchain)
            describe "BlockSignature" $ do
                safeCopyTest @Core.BlockSignature
            describe "ConsensusData" $ do
                safeCopyTest @(Core.ConsensusData Core.MainBlockchain)
            describe "Body" $ do
                safeCopyTest @(Core.Body Core.MainBlockchain)
