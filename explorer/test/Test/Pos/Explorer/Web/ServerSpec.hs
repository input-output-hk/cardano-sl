-- | This module is testing the server api.

{-# LANGUAGE TypeFamilies #-}

module Test.Pos.Explorer.Web.ServerSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (arbitrary, counterexample, forAll, (==>))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Pos.Arbitrary.Block ()
import qualified Pos.Communication ()
import           Pos.Core (EpochIndex (..), HasConfiguration)
import           Pos.Explorer.ExplorerMode (runExplorerTestMode)
import           Pos.Explorer.ExtraContext (ExtraContext (..), makeExtraCtx, makeMockExtraCtx)
import           Pos.Explorer.TestUtil (emptyBlk, generateValidBlocksSlotsNumber,
                                        generateValidExplorerMockableMode, leftToCounter)
import           Pos.Explorer.Web.ClientTypes (CBlockEntry)
import           Pos.Explorer.Web.Server (getBlockDifficulty, getBlocksLastPage, getBlocksPage,
                                          getBlocksPagesTotal, getBlocksTotal, getEpochPage,
                                          getEpochSlot)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util (divRoundUp)
-- Orphan mockable instances.
import           Pos.Util.Mockable ()
import           Test.Pos.Configuration (withDefConfigurations)


----------------------------------------------------------------
-- Spec
----------------------------------------------------------------

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- stack test cardano-sl-explorer --fast --test-arguments "-m Pos.Explorer.Web.Server"
spec :: Spec
spec = withDefConfigurations $ \_ -> do
    describe "Pos.Explorer.Web.Server" $ do
        blocksTotalSpec
        blocksPagesTotalSpec

        blocksTotalUnitSpec
        blocksPagesTotalUnitSpec
        blocksPageUnitSpec
        blocksLastPageUnitSpec

        epochSlotUnitSpec
        epochPageUnitSpec

        blocksTotalFunctionalSpec

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocksTotal could be less than 1.
blocksTotalSpec :: HasConfigurations => Spec
blocksTotalSpec =
    describe "getBlockDifficulty"
    $ modifyMaxSuccess (const 200) $ do
        prop "created blocks means block size > 0" $ emptyBlk $ \blk0 -> leftToCounter blk0 $ \blk ->
            let mainBlock   = Right blk
                blocksTotal = getBlockDifficulty mainBlock
            in counterexample ("Total blocks sizes: " <> show blocksTotal <>
                               "\n\nBlock: " <> show blk) $
                 blocksTotal > 0

-- | A spec with the simple test that @getBlocksPagesTotal@ works correct.
-- It shows that two equal algorithms should work the same.
blocksPagesTotalSpec :: HasConfiguration => Spec
blocksPagesTotalSpec =
    describe "divRoundUp"
    $ modifyMaxSuccess (const 10000) $ do
        prop "valid page number holds" $
            forAll arbitrary $ \(blocksTotal :: Integer, pageSizeInt :: Integer) ->
            -- Otherwise it doesn't make sense.
            blocksTotal >= 1 && pageSizeInt >= 1 ==>
                ((blocksTotal - 1) `div` pageSizeInt) + 1
                    `shouldBe`
                        divRoundUp blocksTotal pageSizeInt

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocksTotal could be different then the number of blocks on the blockchain.
-- I originally thought that this is going to be faster then emulation, but seems the
-- real issue of performance here is the block creation speed.
blocksTotalUnitSpec :: HasConfigurations => Spec
blocksTotalUnitSpec =
    describe "getBlocksTotal"
    $ modifyMaxSuccess (const 200) $ do
        prop "block total == created number of blockchain blocks" $
            forAll arbitrary $ \(testParams) ->
            forAll generateValidBlocksSlotsNumber $ \(totalBlocksNumber, slotsPerEpoch) ->

                monadicIO $ do

                  -- We replace the "real" blockchain with our custom generated one.
                  mode <- lift $ generateValidExplorerMockableMode totalBlocksNumber slotsPerEpoch

                  -- The extra context so we can mock the functions.
                  let extraContext :: ExtraContext
                      extraContext = makeMockExtraCtx mode

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution = runExplorerTestMode testParams extraContext getBlocksTotal

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- run blockExecution

                  -- And we assert that the generated blockchain total block count is equal
                  -- to the expected explorer API result.
                  assert $ blocksTotal == fromIntegral totalBlocksNumber

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocks pages could be < 1.
blocksPagesTotalUnitSpec :: HasConfigurations => Spec
blocksPagesTotalUnitSpec =
    describe "getBlocksPagesTotal"
    $ modifyMaxSuccess (const 200) $ do
        prop "block pages total == created number of paged blockchain blocks" $
            forAll arbitrary $ \(testParams) ->
            forAll generateValidBlocksSlotsNumber $ \(totalBlocksNumber, slotsPerEpoch) ->

                monadicIO $ do

                  -- We replace the "real" blockchain with our custom generated one.
                  mode <- lift $ generateValidExplorerMockableMode totalBlocksNumber slotsPerEpoch

                  -- The extra context so we can mock the functions.
                  let extraContext :: ExtraContext
                      extraContext = makeMockExtraCtx mode

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution =
                          runExplorerTestMode testParams extraContext
                              $ getBlocksPagesTotal (Just 10)

                  -- We finally run it as @PropertyM@ and check if it holds.
                  pagesTotal <- run blockExecution

                  -- And we assert that the generated blockchain total block pages count
                  -- is equal to the expected explorer API result, which is number of
                  -- generated blocks divided by 10.
                  -- It shows that two equal algorithms should work the same.
                  assert $ pagesTotal == (fromIntegral ((totalBlocksNumber - 1) `div` 10) + 1)

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that the client block is not. Also, we can check for specific properties on the
-- client block and they should be relational as to the block.
blocksPageUnitSpec :: HasConfigurations => Spec
blocksPageUnitSpec =
    describe "getBlocksPage"
    $ modifyMaxSuccess (const 200) $ do
        prop "block pages total correct && last page non-empty" $
            forAll arbitrary $ \(testParams) ->
            forAll generateValidBlocksSlotsNumber $ \(totalBlocksNumber, slotsPerEpoch) ->

                monadicIO $ do

                  -- We replace the "real" blockchain with our custom generated one.
                  mode <- lift $ generateValidExplorerMockableMode totalBlocksNumber slotsPerEpoch

                  -- The extra context so we can mock the functions.
                  let extraContext :: ExtraContext
                      extraContext = makeMockExtraCtx mode

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  -- We ask the last page of the blocks pages.
                  let blockExecution :: IO (Integer, [CBlockEntry])
                      blockExecution =
                          runExplorerTestMode testParams extraContext
                              $ getBlocksPage Nothing (Just 10)

                  -- We finally run it as @PropertyM@ and check if it holds.
                  pagesTotal    <- fst <$> run blockExecution
                  cBlockEntries <- snd <$> run blockExecution

                  -- And we assert that the generated blockchain total block pages count
                  -- is equal to the expected explorer API result, which is number of
                  -- generated blocks divided by 10.
                  -- It shows that two equal algorithms should work the same.
                  assert $ pagesTotal == (fromIntegral ((totalBlocksNumber - 1) `div` 10) + 1)
                  -- The last page is never empty.
                  assert $ not . null $ cBlockEntries

                  -- TODO(ks): We can add more invariants to test here, but these are good
                  -- enough for now.


-- | A spec with the following test invariant. If a block is generated, there is no way
-- that the client block is not. Also, we can check for specific properties on the
-- client block and they should be relational as to the block.
blocksLastPageUnitSpec :: HasConfigurations => Spec
blocksLastPageUnitSpec =
    describe "getBlocksLastPage"
    $ modifyMaxSuccess (const 200) $ do
        prop "getBlocksLastPage == getBlocksPage Nothing" $
            forAll arbitrary $ \(testParams) ->
            forAll generateValidBlocksSlotsNumber $ \(totalBlocksNumber, slotsPerEpoch) ->

                monadicIO $ do

                  -- We replace the "real" blockchain with our custom generated one.
                  mode <- lift $ generateValidExplorerMockableMode totalBlocksNumber slotsPerEpoch

                  -- The extra context so we can mock the functions.
                  let extraContext :: ExtraContext
                      extraContext = makeMockExtraCtx mode

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blocksLastPageM :: IO (Integer, [CBlockEntry])
                      blocksLastPageM =
                          runExplorerTestMode testParams extraContext getBlocksLastPage

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  -- We ask the last page of the blocks pages.
                  let blocksPageM :: IO (Integer, [CBlockEntry])
                      blocksPageM =
                          runExplorerTestMode testParams extraContext
                              $ getBlocksPage Nothing (Just 10)

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksLastPage <- run blocksLastPageM
                  blocksPage     <- run blocksPageM

                  -- This function should be equal to calling getBlocksPage with @Nothing@.
                  assert $ blocksLastPage == blocksPage


epochSlotUnitSpec :: HasConfigurations => Spec
epochSlotUnitSpec = do
    describe "getEpochSlot"
    $ modifyMaxSuccess (const 200) $ do
        prop "getEpochSlot(valid epoch) != empty" $
            forAll arbitrary $ \(testParams) ->
            forAll generateValidBlocksSlotsNumber $ \(totalBlocksNumber, slotsPerEpoch) ->

                monadicIO $ do

                  -- We replace the "real" blockchain with our custom generated one.
                  mode <- lift $ generateValidExplorerMockableMode totalBlocksNumber slotsPerEpoch

                  -- The extra context so we can mock the functions.
                  let extraContext :: ExtraContext
                      extraContext = makeMockExtraCtx mode


                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  -- The first slot of the last epoch.
                  -- TODO(ks): Generalize to arbitrary @EpochIndex@.
                  let epochSlotM :: IO [CBlockEntry]
                      epochSlotM =
                          runExplorerTestMode testParams extraContext
                              $ getEpochSlot
                                  (EpochIndex 0)
                                  1

                  -- We finally run it as @PropertyM@ and check if it holds.
                  epochSlot <- run epochSlotM

                  -- The function result should not be empty.
                  assert $ not . null $ epochSlot
                  -- The function result should always be one.
                  assert $ length epochSlot == 1


epochPageUnitSpec :: HasConfigurations => Spec
epochPageUnitSpec = do
    describe "getEpochPage"
    $ modifyMaxSuccess (const 200) $ do
        prop "getEpochPage(valid epoch) != empty" $
            forAll arbitrary $ \(testParams) ->
            forAll generateValidBlocksSlotsNumber $ \(totalBlocksNumber, slotsPerEpoch) ->

                monadicIO $ do

                  -- We replace the "real" blockchain with our custom generated one.
                  mode <- lift $ generateValidExplorerMockableMode totalBlocksNumber slotsPerEpoch

                  -- The extra context so we can mock the functions.
                  let extraContext :: ExtraContext
                      extraContext = makeMockExtraCtx mode

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  -- The first slot of the last epoch.
                  -- TODO(ks): Generalize to arbitrary @EpochIndex@.
                  let epochPageM :: IO (Int, [CBlockEntry])
                      epochPageM =
                          runExplorerTestMode testParams extraContext
                              $ getEpochPage
                                  (EpochIndex 0)
                                  Nothing

                  -- We finally run it as @PropertyM@ and check if it holds.
                  (totalPages, epochPage) <- run epochPageM

                  -- The function result should not be empty.
                  assert $ not . null $ epochPage
                  -- Pages should always exist.
                  assert $ totalPages > 0


-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocksTotal could be less than 0.
-- We don't have control over this block generation, so it really can be 0.
blocksTotalFunctionalSpec :: HasConfigurations => Spec
blocksTotalFunctionalSpec =
    describe "getBlocksTotalFunctional"
    $ modifyMaxSuccess (const 200) $ do
        prop "created blocks means block size >= 0" $
            forAll arbitrary $ \testParams ->
                monadicIO $ do

                  -- The extra context so we can mock the functions.
                  let extraContext :: ExtraContext
                      extraContext = makeExtraCtx

                  -- We run the function in @ExplorerTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution =
                          runExplorerTestMode testParams extraContext getBlocksTotal

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- run blockExecution
                  assert $ blocksTotal >= 0


