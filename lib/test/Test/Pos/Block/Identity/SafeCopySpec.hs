-- | This module tests SafeCopy instances for Block types.

module Test.Pos.Block.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe, runIO)
import           Test.QuickCheck (arbitrary, generate)
import           Universum

import qualified Pos.Core.Block as BT
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.SafeCopy ()

import           Test.Pos.Binary.Helpers (safeCopyTest)
import           Test.Pos.Block.Arbitrary ()
import           Test.Pos.Configuration (withProvidedMagicConfig)

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ describe "Block types" $
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
