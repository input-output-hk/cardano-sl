-- | This module is testing the server api.

{-# LANGUAGE TypeFamilies               #-}

module Test.Pos.Explorer.Web.ServerSpec
       ( spec
       ) where

import           Universum

import           Data.Default               (def)
import           Serokell.Data.Memory.Units (Byte, Gigabyte, convertUnit)
import           Test.Hspec                 (Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck      (modifyMaxSuccess, prop)
import           Test.QuickCheck            (Property, Testable, arbitrary,
                                             counterexample, forAll, property, (==>))
import           Test.QuickCheck.Monadic    (run, monadicIO, assert)

import           Test.Pos.Block.Logic.Mode  (BlockTestMode, runBlockTestMode)

import           Pos.Arbitrary.Block        ()
import           Pos.Block.Core             (Block, BlockHeader, MainBlock)
import           Pos.Block.Logic            (RawPayload (..), createMainBlockPure)
import qualified Pos.Communication          ()
import           Pos.Core                   (HasCoreConstants, SlotId (..),
                                             giveStaticConsts)
import           Pos.Crypto                 (SecretKey)
import           Pos.Delegation             (DlgPayload, ProxySKBlockInfo)
import           Pos.Explorer.Web.Server    (ExplorerMockMode (..),
                                             getBlocksTotal, getBlocksTotalEMode,
                                             pureGetBlocksPagesTotal, pureGetBlocksTotal)
import           Pos.Ssc.Class              (Ssc (..), sscDefaultPayload)
import           Pos.Ssc.GodTossing         (GtPayload (..), SscGodTossing)
import           Pos.Txp.Core               (TxAux)
import           Pos.Update.Core            (UpdatePayload (..))


-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Web.ServerSpec"
spec :: Spec
spec = do
    blocksPureTotalSpec
    blocksPagesPureTotalSpec

    blocksTotalUnitSpec
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
                  -- We replace the "real" database with our custom data.
                  let mode :: ExplorerMockMode BlockTestMode SscGodTossing
                      mode = ExplorerMockMode {
                          eGetTipBlock = pure $ basicBlockGenericUnsafe prevHeader sk slotId
                      }

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution = runBlockTestMode testParams $ getBlocksTotalEMode mode

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- run blockExecution
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

