{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.Block.LogicSpec
       ( spec
       ) where

import           Data.Default               (def)
import           Serokell.Data.Memory.Units (Byte, fromBytes)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Property, Testable, arbitrary, choose,
                                             counterexample, forAll, listOf1, property)
import           Universum

import qualified Pos.Binary.Class           as Bi
import           Pos.Block.Arbitrary        ()
import           Pos.Block.Logic            (createMainBlockPure)
import qualified Pos.Communication          ()
import           Pos.Constants              (blkSecurityParam, genesisMaxBlockSize)
import           Pos.Core                   (SlotId (..))
import           Pos.Crypto                 (SecretKey)
import           Pos.Ssc.Class              (Ssc (..))
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Txp.Core               (TxAux)
import           Pos.Types                  (BlockHeader, MainBlock, ProxySKEither,
                                             ProxySKHeavy, SmallGoodTx (..),
                                             goodTxToTxAux)
import           Pos.Update.Core            (UpdatePayload (..))


spec :: Spec
spec = describe "Block.Logic" $ do
    describe "createMainBlockPure" $ do
        prop "empty block size is sane" $ emptyBlk $ \blk ->
            let s = Bi.biSize blk
            in counterexample ("Real block size: " <> show s) $
               s <= 500 && s <= genesisMaxBlockSize
        prop "doesn't create blocks bigger than the limit" $
            -- 400b is minimum bound for empty block
            forAll (choose (400, 4000)) $ \(fromBytes -> limit) ->
            forAll arbitrary $ \(prevHeader, sk, updatePayload, proxyCerts) ->
            forAll (listOf1 genTx) $ \txs ->
            let blk = noSscBlock limit txs prevHeader sk updatePayload proxyCerts
            in leftToCounter blk $ \b ->
                let s = length (Bi.encode b)
                in counterexample ("Real block size: " <> show s) $
                   s <= fromIntegral limit
        prop "Removes transactions when necessary" $
            forAll arbitrary $ \(prevHeader, sk) ->
            forAll (listOf1 genTx) $ \txs ->
            let blk0 = noSscBlock infLimit [] prevHeader sk def []
            in leftToCounter blk0 $ \b ->
                let s = fromIntegral $ length $ Bi.encode b
                    blk1 = noSscBlock s txs prevHeader sk def []
                in counterexample ("Block size was: " <> show s) $
                   leftToCounter blk1 (const True)
        -- TODO add ssc stripping test
  where
    infLimit = fromIntegral ((10 :: Word64) ^ (9 :: Word64)) :: Byte
    neutralSId = SlotId 0 (blkSecurityParam * 2)

    leftToCounter :: (ToString s, Testable p) => Either s a -> (a -> p) -> Property
    leftToCounter x c = either (\t -> counterexample (toString t) False) (property . c) x

    emptyBlk foo = forAll arbitrary $ \(prevHeader, sk) ->
        foo $ noSscBlock infLimit [] prevHeader sk def []
    genTx = goodTxToTxAux . getSmallGoodTx <$> arbitrary
    noSscBlock limit txs prevHeader sk updatePayload proxyCerts =
        producePureBlock
            limit prevHeader txs Nothing neutralSId proxyCerts def updatePayload sk
    producePureBlock
        :: Byte
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
