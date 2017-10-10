-- | This module tests BListener from 'Pos.Explorer'.

module Test.Pos.Explorer.BListenerSpec
       ( spec
       ) where

import           Universum

import qualified Data.List.NonEmpty        as NE
import           Data.Map                  (keys)
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (modifyMaxSuccess, prop)
import           Test.QuickCheck           (Arbitrary (..), Gen, Property, forAll, listOf,
                                            suchThat, (===))
import           Test.QuickCheck.Monadic   (assert, pre)

import           Pos.Core                  (BlockCount (..), EpochIndex (..), HeaderHash)
import           Pos.Util                  (divRoundUp)

import           Pos.Explorer              (Epoch, convertToPagedMap, epochPagedBlocksMap)
import           Test.Pos.Block.Logic.Mode (BlockProperty, HasVarSpecConfigurations)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..), InplaceDB (..),
                                            bpGenBlock, bpGenBlocks, bpGoToArbitraryState)
import           Test.Pos.Util             (giveCoreConf, giveGtConf, giveInfraConf,
                                            giveNodeConf, giveUpdateConf)



----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl --fast --test-arguments "-m Test.Pos.Explorer.BListener"
spec :: Spec
spec = do
    unitSpec
    functionalSpec

----------------------------------------------------------------------------
-- Explorer functional
----------------------------------------------------------------------------

functionalSpec :: Spec
functionalSpec = giveGtConf $ giveNodeConf $ giveInfraConf $ giveUpdateConf $ giveCoreConf $
    describe "Pos.Explorer.BListenerSpec" $ modifyMaxSuccess (min 12) $ do
        describe "verifyBlocksExplorer" verifyBlocksPrefixExplorerSpec

verifyBlocksPrefixExplorerSpec
    :: HasVarSpecConfigurations => Spec
verifyBlocksPrefixExplorerSpec = do
    prop verifyCorrectNumberOfEpochsEmptyBlocks verifyEmptyValidBlockExplorer
    prop verifyCorrectNumberOfEpochsBlocks verifyValidBlocksExplorer
  where
    verifyCorrectNumberOfEpochsEmptyBlocks = "explorer correct number of empty epoch pages"
    verifyCorrectNumberOfEpochsBlocks = "explorer correct number of epoch pages"

verifyEmptyValidBlockExplorer
    :: HasVarSpecConfigurations
    => BlockProperty ()
verifyEmptyValidBlockExplorer = do
    emptyBlock     <- fst <$> bpGenBlock (EnableTxPayload False) (InplaceDB False)
    let epochBlocksMap = epochPagedBlocksMap $ NE.fromList [emptyBlock]
    assert $ length epochBlocksMap == 1

verifyValidBlocksExplorer
    :: HasVarSpecConfigurations => BlockProperty ()
verifyValidBlocksExplorer = do

    bpGoToArbitraryState

    blocks <-
        map fst . toList <$>
        bpGenBlocks blockNumber (EnableTxPayload False) (InplaceDB False)
    pre (not $ null blocks)

    let epochBlocksMap   = epochPagedBlocksMap $ NE.fromList blocks

    assert $ (length $ fst <$> keys epochBlocksMap) == 10
    assert $ (length $ snd <$> keys epochBlocksMap) == 10
  where
    blockNumber :: Maybe BlockCount
    blockNumber = Just $ BlockCount 100

----------------------------------------------------------------------------
-- Explorer unit
----------------------------------------------------------------------------

unitSpec :: Spec
unitSpec =
    describe "Pos.Explorer.BListenerSpec" $ do
        modifyMaxSuccess (const 10000) $ do
            prop "convertToPagedMap" verifyConversionToPagedMap

verifyConversionToPagedMap :: Property
verifyConversionToPagedMap =
  forAll arbitrary $ \epochHeaderHashes ->
      (length $ convertToPagedMap $ getEpochHeaderHashes epochHeaderHashes) ===
          (divRoundUp (length $ snd $ getEpochHeaderHashes epochHeaderHashes) 10)

----------------------------------------------------------------------------
-- Arbitrary
----------------------------------------------------------------------------

-- | Single epoch.
newtype EpochHeaderHashes = EpochHeaderHashes
    { getEpochHeaderHashes :: (Epoch, [HeaderHash])
    } deriving Show

instance Arbitrary EpochHeaderHashes where
    arbitrary = do
        epochIndex   <- arbitrary
        -- The invariant is that an epoch can't have empty header hashes, which is
        -- reasonable.
        headerHashes <- suchThat arbitrary (\(hhs) -> length hhs > 0)
        pure $ EpochHeaderHashes (epochIndex, headerHashes)

-- | Multiple epochs.
newtype EpochsHeaderHashes = EpochsHeaderHashes
    { _getEpochsHeaderHashes :: [(Epoch, [HeaderHash])]
    } deriving Show

instance Arbitrary EpochsHeaderHashes where
    arbitrary = do
        genHeaderHashes <- listOfGeneratedHeaderHashes
        pure $ EpochsHeaderHashes $ zip [firstEpochIndex..] genHeaderHashes
      where
        firstEpochIndex :: EpochIndex
        firstEpochIndex = minBound

        listOfGeneratedHeaderHashes :: Gen [[HeaderHash]]
        listOfGeneratedHeaderHashes = listOf generatedHeaderHashes

        -- | The invariant is that an epoch can't have empty header hashes, which is
        -- reasonable.
        generatedHeaderHashes :: Gen [HeaderHash]
        generatedHeaderHashes = suchThat arbitrary (\(hhs) -> length hhs > 0)