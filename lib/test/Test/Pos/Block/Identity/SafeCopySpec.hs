-- | This module tests SafeCopy instances for Block types.

module Test.Pos.Block.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (generate)
import           Universum

import qualified Pos.Core.Block as BT
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.SafeCopy ()

import           Test.Pos.Binary.Helpers (safeCopyTest)
import           Test.Pos.Block.Arbitrary ()
import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Crypto.Arbitrary (genProtocolMagicUniformWithRNM)


-- We run the tests this number of times, with different `ProtocolMagics`, to get increased
-- coverage. We should really do this inside of the `prop`, but it is difficult to do that
-- without significant rewriting of the testsuite.
testMultiple :: Int
testMultiple = 3

spec :: Spec
spec = do
    runWithMagic RequiresNoMagic
    runWithMagic RequiresMagic

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = replicateM_ testMultiple $
    modifyMaxSuccess (`div` testMultiple) $ do
        pm <- runIO (generate (genProtocolMagicUniformWithRNM rnm))
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
