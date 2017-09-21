-- | This module is testing the server api.

{-# LANGUAGE TypeFamilies #-}

module Test.Pos.Explorer.Web.ServerSpec
       ( spec
       ) where

import qualified Prelude
import           Universum

import           Data.Default                      (def)
import           Data.Text.Buildable               (build)
import           Data.List.NonEmpty                (fromList)
import           Serokell.Data.Memory.Units        (Byte, Gigabyte, convertUnit)
import           Test.Hspec                        (Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck             (modifyMaxSuccess, prop)
import           Test.QuickCheck                   (Arbitrary, Property, Testable,
                                                    arbitrary, counterexample, forAll,
                                                    property, (==>))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import           Test.QuickCheck.Monadic           (assert, monadicIO, run)

import           Test.Pos.Block.Logic.Mode         (BlockTestMode, runBlockTestMode)

import           Pos.Arbitrary.Block               ()
import           Pos.Block.Core                    (Block, BlockHeader, MainBlock)
import           Pos.Block.Logic                   (RawPayload (..), createMainBlockPure)
import           Pos.Block.Types                   (SlogUndo, Undo)
import qualified Pos.Communication                 ()
import           Pos.Core                          (HasCoreConstants, SlotId (..),
                                                    EpochIndex (..), LocalSlotIndex (..),
                                                    giveStaticConsts)
import           Pos.Crypto                        (SecretKey)
import           Pos.DB.Block                      (MonadBlockDB)
import           Pos.Delegation                    (DlgPayload, DlgUndo, ProxySKBlockInfo)
import           Pos.Explorer.Web.ClientTypes      (CBlockEntry, ExplorerMockMode (..))
import           Pos.Explorer.Web.Server           (getBlocksLastPageEMode,
                                                    getBlocksPageEMode,
                                                    getBlocksPagesTotalEMode,
                                                    getBlocksTotal, getBlocksTotalEMode,
                                                    pureGetBlocksPagesTotal,
                                                    pureGetBlocksTotal)
import           Pos.Ssc.Class                     (Ssc (..), sscDefaultPayload)
import           Pos.Ssc.GodTossing                (GtPayload (..), SscGodTossing)
import           Pos.Txp.Core                      (TxAux)
import           Pos.Update.Core                   (UpdatePayload (..))



----------------------------------------------------------------
-- Spec
----------------------------------------------------------------

-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Web.ServerSpec"
spec :: Spec
spec = do
    blocksPureTotalSpec
    blocksPagesPureTotalSpec

    blocksTotalUnitSpec
    blocksPagesTotalUnitSpec
    blocksPageUnitSpec
    blocksLastPageUnitSpec

    blocksTotalFunctionalSpec

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocksTotal could be less than 1.
blocksPureTotalSpec :: Spec
blocksPureTotalSpec = giveStaticConsts
    $ describe "pureGetBlocksTotal"
    $ modifyMaxSuccess (const 200) $ do
        prop "created blocks means block size > 0" $ emptyBlk $ \blk0 -> leftToCounter blk0 $ \blk ->
            let mainBlock   = Right blk
                blocksTotal = pureGetBlocksTotal mainBlock
            in counterexample ("Total blocks sizes: " <> show blocksTotal <>
                               "\n\nBlock: " <> show blk) $
                 blocksTotal > 0

-- | A spec with the simple test that two equal algorithms should work the same.
blocksPagesPureTotalSpec :: Spec
blocksPagesPureTotalSpec = giveStaticConsts
    $ describe "pureGetBlocksPagesTotal"
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
blocksTotalUnitSpec :: Spec
blocksTotalUnitSpec = giveStaticConsts
    $ describe "getBlocksTotalUnit"
    $ modifyMaxSuccess (const 200) $ do
        prop "created blocks means block size > 0" $
            forAll arbitrary $ \(testParams, prevHeader, sk, slotId) ->
                monadicIO $ do

                  -- The created arbitrary block.
                  let testBlock :: MonadBlockDB SscGodTossing m => m (Block SscGodTossing)
                      testBlock = pure $ basicBlockGenericUnsafe prevHeader sk slotId

                  -- The default @ExplorerMockMode@ instance that has no implementation.
                  let defaultInstance :: ExplorerMockMode BlockTestMode SscGodTossing
                      defaultInstance = def

                  -- We replace the "real" database with our custom data.
                  let mode :: ExplorerMockMode BlockTestMode SscGodTossing
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
blocksPagesTotalUnitSpec :: Spec
blocksPagesTotalUnitSpec = giveStaticConsts
    $ describe "getBlocksPagesTotal"
    $ modifyMaxSuccess (const 200) $ do
        prop "block pages = 1" $
            forAll arbitrary $ \(testParams, prevHeader, sk, slotId) ->
                monadicIO $ do

                  -- The created arbitrary block.
                  let testBlock :: MonadBlockDB SscGodTossing m => m (Block SscGodTossing)
                      testBlock = pure $ basicBlockGenericUnsafe prevHeader sk slotId

                  -- The default @ExplorerMockMode@ instance that has no implementation.
                  let defaultInstance :: ExplorerMockMode BlockTestMode SscGodTossing
                      defaultInstance = def

                  -- We replace the "real" database with our custom data.
                  let mode :: ExplorerMockMode BlockTestMode SscGodTossing
                      mode = defaultInstance { emmGetTipBlock = testBlock }

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution = runBlockTestMode testParams $ getBlocksPagesTotalEMode mode 10

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- run blockExecution
                  assert $ blocksTotal > 0

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that the client block is not. Also, we can check for specific properties on the
-- client block and they should be relational as to the block.
blocksPageUnitSpec :: Spec
blocksPageUnitSpec = giveStaticConsts
    $ describe "getBlocksPage"
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
                  let testBlock :: MonadBlockDB SscGodTossing m => m (Block SscGodTossing)
                      testBlock = pure $ basicBlockGenericUnsafe prevHeader sk mockSlotId

                  -- The default @ExplorerMockMode@ instance that has no implementation.
                  let defaultInstance :: ExplorerMockMode BlockTestMode SscGodTossing
                      defaultInstance = def

                  -- We replace the "real" functions with our custom functions.
                  let mode :: ExplorerMockMode BlockTestMode SscGodTossing
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
                          runBlockTestMode testParams $ getBlocksPageEMode mode Nothing 10

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- fst <$> run blockExecution
                  assert $ blocksTotal > 0

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that the client block is not. Also, we can check for specific properties on the
-- client block and they should be relational as to the block.
blocksLastPageUnitSpec :: Spec
blocksLastPageUnitSpec = giveStaticConsts
    $ describe "getBlocksLastPage"
    $ modifyMaxSuccess (const 200) $ do
        prop "count(block) > 0" $
            forAll arbitrary $ \(testParams, prevHeader, sk, slotId) ->
            forAll arbitrary $ \(timestamp, blund, leader, hh) ->
                monadicIO $ do

                  -- The created arbitrary block.
                  let testBlock :: MonadBlockDB SscGodTossing m => m (Block SscGodTossing)
                      testBlock = pure $ basicBlockGenericUnsafe prevHeader sk slotId

                  -- The default @ExplorerMockMode@ instance that has no implementation.
                  let defaultInstance :: ExplorerMockMode BlockTestMode SscGodTossing
                      defaultInstance = def

                  -- We replace the "real" functions with our custom functions.
                  let mode :: ExplorerMockMode BlockTestMode SscGodTossing
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
blocksTotalFunctionalSpec :: Spec
blocksTotalFunctionalSpec = giveStaticConsts
    $ describe "getBlocksTotalFunctional"
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

----------------------------------------------------------------
-- Arbitrary and Show instances
----------------------------------------------------------------

-- I used the build function since I suspect that it's safe (even in tests).
instance HasCoreConstants => Prelude.Show SlogUndo where
    show = show . build

instance Prelude.Show DlgUndo where
    show = show . build

instance HasCoreConstants => Prelude.Show Undo where
    show = show . build

instance Arbitrary SlogUndo where
    arbitrary = genericArbitrary

instance Arbitrary DlgUndo where
    arbitrary = genericArbitrary

instance HasCoreConstants => Arbitrary Undo where
    arbitrary = genericArbitrary

----------------------------------------------------------------
-- Utility
-- TODO(ks): Extract this in some common PureBlockTest module in src/?
----------------------------------------------------------------

basicBlockGenericUnsafe
    :: (HasCoreConstants)
    => BlockHeader SscGodTossing
    -> SecretKey
    -> SlotId
    -> Block SscGodTossing
basicBlockGenericUnsafe prevHeader sk slotId = case (basicBlock prevHeader sk slotId) of
    Left e      -> error e
    Right block -> Right block

basicBlock
    :: (HasCoreConstants)
    => BlockHeader SscGodTossing
    -> SecretKey
    -> SlotId
    -> Either Text (MainBlock SscGodTossing)
basicBlock prevHeader sk slotId =
    producePureBlock infLimit prevHeader [] Nothing slotId def (defGTP slotId) def sk
  where
    defGTP :: HasCoreConstants => SlotId -> GtPayload
    defGTP sId = sscDefaultPayload @SscGodTossing $ siSlot sId

    infLimit = convertUnit @Gigabyte @Byte 1

emptyBlk :: (HasCoreConstants, Testable p) => (Either Text (MainBlock SscGodTossing) -> p) -> Property
emptyBlk testableBlock =
    forAll arbitrary $ \(sk, prevHeader, slotId) ->
    testableBlock
        $ producePureBlock infLimit prevHeader [] Nothing slotId def (defGTP slotId) def sk
  where
    defGTP :: HasCoreConstants => SlotId -> GtPayload
    defGTP sId = sscDefaultPayload @SscGodTossing $ siSlot sId

    infLimit = convertUnit @Gigabyte @Byte 1

producePureBlock
    :: HasCoreConstants
    => Byte
    -> BlockHeader SscGodTossing
    -> [TxAux]
    -> ProxySKBlockInfo
    -> SlotId
    -> DlgPayload
    -> SscPayload SscGodTossing
    -> UpdatePayload
    -> SecretKey
    -> Either Text (MainBlock SscGodTossing)
producePureBlock limit prev txs psk slot dlgPay sscPay usPay sk =
    createMainBlockPure limit prev psk slot sk $
    RawPayload txs sscPay dlgPay usPay

leftToCounter :: (ToString s, Testable p) => Either s a -> (a -> p) -> Property
leftToCounter x c = either (\t -> counterexample (toString t) False) (property . c) x

