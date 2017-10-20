-- | This module is testing the server api.

{-# LANGUAGE TypeFamilies #-}

module Test.Pos.Explorer.Web.ServerSpec
       ( spec
       ) where

import           Universum

import           Data.Default                 (def)
import           Data.List.NonEmpty           (fromList)
import           Test.Hspec                   (Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck        (modifyMaxSuccess, prop)
import           Test.QuickCheck              (arbitrary, counterexample, forAll, (==>))
import           Test.QuickCheck.Monadic      (assert, monadicIO, run)

import           Test.Pos.Block.Logic.Mode    (BlockTestMode, runBlockTestMode)

import           Pos.Arbitrary.Block          ()
import           Pos.Block.Core               (Block)
import qualified Pos.Communication            ()
import           Pos.Core                     (EpochIndex (..), HasConfiguration,
                                               LocalSlotIndex (..), SlotId (..))
import           Pos.DB.Block                 (MonadBlockDB)
import           Pos.Explorer.TestUtil        (basicBlockGenericUnsafe, emptyBlk,
                                               leftToCounter)
import           Pos.Explorer.Web.ClientTypes (CBlockEntry, ExplorerMockMode (..))
import           Pos.Explorer.Web.Server      (getBlocksLastPageEMode, getBlocksPageEMode,
                                               getBlocksPagesTotalEMode, getBlocksTotal,
                                               getBlocksTotalEMode,
                                               pureGetBlocksPagesTotal,
                                               pureGetBlocksTotal)

import           Pos.Launcher.Configuration   (HasConfigurations)
import           Test.Pos.Util                (withDefConfigurations)



----------------------------------------------------------------
-- Spec
----------------------------------------------------------------

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- sack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Web.ServerSpec"
spec :: Spec
spec = withDefConfigurations $ do
    describe "Pos.Explorer.Web.Server" $ do
        blocksPureTotalSpec
        blocksPagesPureTotalSpec

        blocksTotalUnitSpec
        blocksPagesTotalUnitSpec
        blocksPageUnitSpec
        blocksLastPageUnitSpec

        blocksTotalFunctionalSpec

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocksTotal could be less than 1.
blocksPureTotalSpec :: HasConfigurations => Spec
blocksPureTotalSpec =
    describe "pureGetBlocksTotal"
    $ modifyMaxSuccess (const 200) $ do
        prop "created blocks means block size > 0" $ emptyBlk $ \blk0 -> leftToCounter blk0 $ \blk ->
            let mainBlock   = Right blk
                blocksTotal = pureGetBlocksTotal mainBlock
            in counterexample ("Total blocks sizes: " <> show blocksTotal <>
                               "\n\nBlock: " <> show blk) $
                 blocksTotal > 0

-- | A spec with the simple test that two equal algorithms should work the same.
blocksPagesPureTotalSpec :: HasConfiguration => Spec
blocksPagesPureTotalSpec =
    describe "pureGetBlocksPagesTotal"
    $ modifyMaxSuccess (const 10000) $ do
        prop "valid page number holds" $
            forAll arbitrary $ \(blocksTotal, pageSizeInt) ->
            -- Otherwise it doesn't make sense.
            blocksTotal >= 1 && pageSizeInt >= 1 ==>
                ((blocksTotal - 1) `div` pageSizeInt) + 1
                    `shouldBe`
                        pureGetBlocksPagesTotal blocksTotal pageSizeInt

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocksTotal could be less than 1.
-- I originally thought that this is going to be faster then emulation, but seems the
-- real issue of performance here is the block creation speed.
blocksTotalUnitSpec :: HasConfigurations => Spec
blocksTotalUnitSpec =
    describe "getBlocksTotalUnit"
    $ modifyMaxSuccess (const 200) $ do
        prop "created blocks means block size > 0" $
            forAll arbitrary $ \(testParams, prevHeader, sk, slotId) ->
                monadicIO $ do

                  -- The created arbitrary block.
                  let testBlock :: MonadBlockDB m => m Block
                      testBlock = pure $ basicBlockGenericUnsafe prevHeader sk slotId

                  -- The default @ExplorerMockMode@ instance that has no implementation.
                  let defaultInstance :: ExplorerMockMode BlockTestMode
                      defaultInstance = def

                  -- We replace the "real" database with our custom data.
                  let mode :: ExplorerMockMode BlockTestMode
                      mode = defaultInstance { emmGetTipBlock = testBlock }

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution = runBlockTestMode testParams $ getBlocksTotalEMode mode

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- run blockExecution
                  assert $ blocksTotal > 0

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocks pages could be < 1.
blocksPagesTotalUnitSpec :: HasConfigurations => Spec
blocksPagesTotalUnitSpec =
    describe "getBlocksPagesTotal"
    $ modifyMaxSuccess (const 200) $ do
        prop "block pages = 1" $
            forAll arbitrary $ \(testParams, prevHeader, sk, slotId) ->
                monadicIO $ do

                  -- The created arbitrary block.
                  let testBlock :: MonadBlockDB m => m Block
                      testBlock = pure $ basicBlockGenericUnsafe prevHeader sk slotId

                  -- The default @ExplorerMockMode@ instance that has no implementation.
                  let defaultInstance :: ExplorerMockMode BlockTestMode
                      defaultInstance = def

                  -- We replace the "real" database with our custom data.
                  let mode :: ExplorerMockMode BlockTestMode
                      mode = defaultInstance { emmGetTipBlock = testBlock }

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution =
                          runBlockTestMode testParams
                          $ getBlocksPagesTotalEMode mode (Just 10)

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- run blockExecution
                  assert $ blocksTotal > 0

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that the client block is not. Also, we can check for specific properties on the
-- client block and they should be relational as to the block.
blocksPageUnitSpec :: HasConfigurations => Spec
blocksPageUnitSpec =
    describe "getBlocksPage"
    $ modifyMaxSuccess (const 200) $ do
        prop "count(block) > 0" $
            forAll arbitrary $ \(testParams, prevHeader, sk) ->
            forAll arbitrary $ \(timestamp, blund, leader, hh) ->
                monadicIO $ do

                  -- A mock slot Id
                  let mockSlotId :: SlotId
                      mockSlotId = SlotId
                          { siEpoch = EpochIndex 0
                          , siSlot  = UnsafeLocalSlotIndex 1
                          }

                  -- The created arbitrary block.
                  let testBlock :: MonadBlockDB m => m Block
                      testBlock = pure $ basicBlockGenericUnsafe prevHeader sk mockSlotId

                  -- The default @ExplorerMockMode@ instance that has no implementation.
                  let defaultInstance :: ExplorerMockMode BlockTestMode
                      defaultInstance = def

                  -- We replace the "real" functions with our custom functions.
                  let mode :: ExplorerMockMode BlockTestMode
                      mode = defaultInstance {
                          emmGetTipBlock            = testBlock,
                          emmGetPageBlocks          = \_   -> pure $ Just hh,
                          emmGetBlundFromHH         = \_   -> pure $ Just blund,
                          emmGetSlotStart           = \_   -> pure $ Just timestamp,
                          emmGetLeadersFromEpoch    = \_   ->
                              pure . Just . fromList $ [leader]
                      }

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO (Integer, [CBlockEntry])
                      blockExecution =
                          runBlockTestMode testParams
                          $ getBlocksPageEMode mode Nothing (Just 10)

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- fst <$> run blockExecution
                  assert $ blocksTotal > 0

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that the client block is not. Also, we can check for specific properties on the
-- client block and they should be relational as to the block.
blocksLastPageUnitSpec :: HasConfigurations => Spec
blocksLastPageUnitSpec =
    describe "getBlocksLastPage"
    $ modifyMaxSuccess (const 200) $ do
        prop "count(block) > 0" $
            forAll arbitrary $ \(testParams, prevHeader, sk, slotId) ->
            forAll arbitrary $ \(timestamp, blund, leader, hh) ->
                monadicIO $ do

                  -- The created arbitrary block.
                  let testBlock :: MonadBlockDB m => m Block
                      testBlock = pure $ basicBlockGenericUnsafe prevHeader sk slotId

                  -- The default @ExplorerMockMode@ instance that has no implementation.
                  let defaultInstance :: ExplorerMockMode BlockTestMode
                      defaultInstance = def

                  -- We replace the "real" functions with our custom functions.
                  let mode :: ExplorerMockMode BlockTestMode
                      mode = defaultInstance {
                          emmGetTipBlock            = testBlock,
                          emmGetPageBlocks          = \_   -> pure $ Just hh,
                          emmGetBlundFromHH         = \_   -> pure $ Just blund,
                          emmGetSlotStart           = \_   -> pure $ Just timestamp,
                          emmGetLeadersFromEpoch    = \_   -> pure $ Just leader
                      }

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO (Integer, [CBlockEntry])
                      blockExecution =
                          runBlockTestMode testParams $ getBlocksLastPageEMode mode

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- fst <$> run blockExecution
                  assert $ blocksTotal > 0


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

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution = runBlockTestMode testParams getBlocksTotal

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- run blockExecution
                  assert $ blocksTotal >= 0