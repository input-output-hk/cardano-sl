{-# LANGUAGE TypeApplications #-}

module Test.Pos.Chain.Block.CborSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)

import qualified Pos.Chain.Block as Core
import           Pos.Core.Configuration (defaultCoreConfiguration,
                     withGenesisSpec)

import           Test.Pos.Binary.Helpers (binaryTest)
import           Test.Pos.Chain.Block.Arbitrary ()
import           Test.Pos.Core.Arbitrary ()

spec :: Spec
spec = withGenesisSpec 0 defaultCoreConfiguration id $ \_ ->
    describe "Cbor.Bi instances" $ do
        -- These data types are defined in the 'core' package which suggests that
        -- these tests should be there, but they depend on type in eg 'update' so
        -- that is not possible.
        -- In addition, these types cannot be moved to the 'core package because
        -- the 'MainBlockHeader' type is used in the 'infra' package and 'block'
        -- depends on that.
        -- Would like to disentangle this at some stage.
        describe "Block types defined in the core package" $ do
              modifyMaxSuccess (min 10) $ describe "GenericBlockHeader" $ do
                  describe "GenesisBlockHeader" $ do
                      binaryTest @Core.GenesisBlockHeader
                  describe "MainBlockHeader" $ do
                      binaryTest @Core.MainBlockHeader
              describe "GenesisBlockchain" $ do
                  describe "BodyProof" $ do
                      binaryTest @Core.GenesisExtraHeaderData
                      binaryTest @Core.GenesisExtraBodyData
                      binaryTest @(Core.BodyProof Core.GenesisBlockchain)
                  describe "ConsensusData" $ do
                      binaryTest @(Core.ConsensusData Core.GenesisBlockchain)
                  describe "Body" $ do
                      binaryTest @(Core.Body Core.GenesisBlockchain)
              describe "MainBlockchain" $ do
                  describe "BodyProof" $ do
                      binaryTest @(Core.BodyProof Core.MainBlockchain)
                  describe "BlockSignature" $ do
                      binaryTest @Core.BlockSignature
                  describe "ConsensusData" $ do
                      binaryTest @(Core.ConsensusData Core.MainBlockchain)
                  modifyMaxSuccess (min 10) $ describe "Body" $ do
                      binaryTest @(Core.Body Core.MainBlockchain)
                  describe "MainToSign" $ do
                      binaryTest @Core.MainToSign
                  describe "Extra data" $ do
                      binaryTest @Core.MainExtraHeaderData
                      binaryTest @Core.MainExtraBodyData
