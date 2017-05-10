{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.Block.BlockSpec
       ( spec
       ) where

import qualified Data.HashMap.Strict   as HM
import           Test.Hspec            (Spec, describe, hspec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, Testable, arbitrary, choose,
                                        counterexample, forAll, listOf1)
import           Universum

import qualified Pos.Binary.Class      as Bi
import           Pos.Block.Arbitrary   ()
import           Pos.Block.Logic       (createMainBlockPure)
import qualified Pos.Communication     ()
import           Pos.Constants         (blkSecurityParam)
import           Pos.Core              (SlotId (..))
import           Pos.Crypto            (SecretKey)
import           Pos.Ssc.Class         (Ssc (..))
import           Pos.Ssc.GodTossing    (GtPayload (..), SscGodTossing)
import           Pos.Txp.Core          (TxAux)
import           Pos.Types             (BlockHeader, MainBlock, ProxySKEither,
                                        ProxySKHeavy, SmallGoodTx (..), goodTxToTxAux)
import           Pos.Update.Core       (UpdatePayload (..))


spec :: Spec
spec = describe "Block.Logic" $ do
    describe "Costruction limits" $ do
        prop "createMainBLockPure doesn't create blocks bigger than the limit" $
            forAll (choose (400, 4000)) $ \(limit :: Word64) ->
            forAll (listOf1 genTx) $ \txs ->
            prodBlockProp limit txs $
            either (\t -> counterexample (toString t) False)
                   (\b -> let s = length (Bi.encode b)
                          in counterexample ("Real block size: " <> show s) $
                             s <= fromIntegral limit)
--        prop "Blocks w/o ssc truncate transactions correctly" $
--            -- 400 bytes is empty block size
--            forAll (choose (400, 4000)) $ \(limit :: Word64) ->
--            forAll (listOf1 genTx) $ \txs ->
--            prodBlockProp limit [] $ \blk0 ->
--            prodBlockProp limit txs $ \blk1 ->
--                traceShow limit $
--                counterexample (show blk) (isRight blk)
  where
    genTx = goodTxToTxAux . getSmallGoodTx <$> arbitrary
    prodBlockProp
        :: (Testable p)
        => Word64 -> [TxAux] -> (Either Text (MainBlock SscGodTossing) -> p) -> Property
    prodBlockProp limit txs cont =
        forAll arbitrary $ \(prevHeader, sk, updatePayload, proxyCerts) ->
            let slotId = SlotId 0 (blkSecurityParam * 2)
            in cont $ producePureBlock
                       limit prevHeader txs Nothing slotId proxyCerts
                       (CertificatesPayload HM.empty) updatePayload sk
    producePureBlock
        :: Word64
        -> BlockHeader SscGodTossing
        -> [TxAux]
        -> Maybe ProxySKEither
        -> SlotId
        -> [ProxySKHeavy]
        -> SscPayload SscGodTossing
        -> UpdatePayload
        -> SecretKey
        -> Either Text (MainBlock SscGodTossing)
    producePureBlock = createMainBlockPure
