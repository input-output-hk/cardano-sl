-- | This module tests BListener from 'Pos.Explorer'.

module Test.Pos.Explorer.BListenerSpec
       ( spec
       ) where

import           Universum                    hiding ((<>))

import           Data.Map                     (keys)--, elems)
import qualified Data.List.NonEmpty           as NE
import           Data.Semigroup               ((<>))
import           Test.Hspec                   (Spec, describe)
import           Test.Hspec.QuickCheck        (modifyMaxSuccess, prop)
import           Test.QuickCheck.Monadic      (assert, pre)

import           Pos.Core                     (BlockCount (..))--, epochIndexL, EpochIndex (..))

import           Test.Pos.Block.Logic.Mode    (BlockProperty, HasVarSpecConfigurations)
import           Test.Pos.Block.Logic.Util    (EnableTxPayload (..), InplaceDB (..),
                                               bpGenBlock, bpGenBlocks,
                                               bpGoToArbitraryState)
import           Test.Pos.Util                (giveCoreConf, giveGtConf, giveInfraConf,
                                               giveNodeConf, giveUpdateConf)
import           Pos.Explorer                 (epochPagedBlocksMap)


-- stack test cardano-sl --fast --test-arguments "-m Test.Pos.Explorer.BListener"
spec :: Spec
spec = giveGtConf $ giveNodeConf $ giveInfraConf $ giveUpdateConf $ giveCoreConf $
    describe "Pos.Explorer.BListenerSpec" $ modifyMaxSuccess (min 12) $ do
        describe "verifyBlocksExplorerPrefix" verifyBlocksPrefixExplorerSpec

----------------------------------------------------------------------------
-- Explorer
----------------------------------------------------------------------------

verifyBlocksPrefixExplorerSpec
    :: HasVarSpecConfigurations => Spec
verifyBlocksPrefixExplorerSpec = do
    prop verifyEmptyMainBlockDesc verifyEmptyMainBlockExplorer
    prop "explorer correct number of epochs" verifyValidBlocksExplorer
  where
    verifyEmptyMainBlockDesc =
        "verification of consistent empty main block " <>
        "created by the leader of the 0-th slot " <>
        "always succeeds for initial GState"

verifyEmptyMainBlockExplorer
    :: HasVarSpecConfigurations
    => BlockProperty ()
verifyEmptyMainBlockExplorer = do
    emptyBlock     <- fst <$> bpGenBlock (EnableTxPayload False) (InplaceDB False)
    let epochBlocksMap = epochPagedBlocksMap $ NE.fromList [emptyBlock]
    assert $ length epochBlocksMap == 1

verifyValidBlocksExplorer
    :: HasVarSpecConfigurations => BlockProperty ()
verifyValidBlocksExplorer = do

    let blockCount :: Int
        blockCount = 100

    bpGoToArbitraryState

    blocks <-
        map fst . toList <$>
        bpGenBlocks (Just $ BlockCount $ fromIntegral blockCount) (EnableTxPayload False) (InplaceDB False)
    pre (not $ null blocks)

    let epochBlocksMap   = epochPagedBlocksMap $ NE.fromList blocks

    assert $ (length $ fst <$> keys epochBlocksMap) == 10
    assert $ (length $ snd <$> keys epochBlocksMap) == 10
