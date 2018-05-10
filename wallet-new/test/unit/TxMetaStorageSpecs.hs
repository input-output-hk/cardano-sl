{-# LANGUAGE RankNTypes #-}
module TxMetaStorageSpecs (txMetaStorageSpecs) where

import           Universum

import           Cardano.Wallet.Kernel.DB.TxMeta
import           Control.Exception.Safe (bracket)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List.Split as Split
import           Data.Text.Buildable (build)
import qualified Prelude

import qualified Pos.Core as Core

import           Formatting (bprint)
import           Serokell.Util.Text (listJsonIndent)
import           Test.Hspec (shouldThrow)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, forAll, vectorOf)
import           Test.QuickCheck.Monadic (monadicIO, pick, run)
import           Util.Buildable (ShowThroughBuild (..))
import           Util.Buildable.Hspec

-- | Handy combinator which yields a fresh database to work with on each spec.
withTemporaryDb :: forall m a. (MonadIO m, MonadMask m) => (MetaDBHandle -> m a) -> m a
withTemporaryDb action = bracket acquire release action
    where
       acquire :: m MetaDBHandle
       acquire = liftIO $ do
               db <- openMetaDB ":memory:"
               migrateMetaDB db
               return db

       release :: MetaDBHandle -> m ()
       release = liftIO . closeMetaDB


-- | Generates two 'TxMeta' which are @almost@ identical, if not in the
-- arrangement of their inputs & outputs.
genSimilarTxMetas :: Gen (ShowThroughBuild TxMeta, ShowThroughBuild TxMeta)
genSimilarTxMetas = do
    inputs  <- uniqueElements 5
    outputs <- uniqueElements 5
    blueprint <- arbitrary
    let t1 = blueprint & over txMetaInputs  (const inputs)
                       . over txMetaOutputs (const outputs)
    let t2 = blueprint & over txMetaInputs  (const (NonEmpty.reverse inputs))
                       . over txMetaOutputs (const (NonEmpty.reverse outputs))
    return (STB t1, STB t2)

-- | Synthetic @newtype@ used to generate unique inputs and outputs as part of
-- 'genMetas'. The reason why it is necessary is because the stock implementation
-- of 'Eq' for '(Core.Address, Core.Coin)' would of course declare two tuples
-- equal if their elements are.
-- However, this is too \"strong\" for our 'uniqueElements' generator, which
-- would consider these two unique:
--
-- ("123", 10)
-- ("123", 0)
--
-- This would of course break our persistent storage, because inserting "123"
-- twice would trigger the primary key uniqueness violation.
newtype TxEntry = TxEntry { getTxEntry :: (Core.Address, Core.Coin) }

instance Eq TxEntry where
    (TxEntry (a1, _)) == (TxEntry (a2, _)) = a1 == a2

instance Ord TxEntry where
    compare (TxEntry (_, c1)) (TxEntry (_, c2)) = compare c1 c2

instance Arbitrary TxEntry where
    arbitrary = TxEntry  <$> arbitrary

-- | Handy generator which make sure we are generating 'TxMeta' which all
-- have distinct inputs and outptus.
genMetas :: Int -> Gen [ShowThroughBuild TxMeta]
genMetas size = do
    metas  <- vectorOf size arbitrary
    inputs  <- Split.chunksOf 5 . toList <$> uniqueElements (length metas * 5)
    outputs <- Split.chunksOf 5 . toList <$> uniqueElements (length metas * 5)
    return $ map (STB . mkTx) (Prelude.zip3 metas inputs outputs)

    where
        mkTx :: (TxMeta, [TxEntry], [TxEntry])
             -> TxMeta
        mkTx (tMeta, i, o) =
            case liftM2 (,) (nonEmpty . map getTxEntry $ i) (nonEmpty . map getTxEntry $ o) of
                 Nothing -> error "mkTx: the impossible happened, invariant violated."
                 Just (inputs, outputs) ->
                     tMeta & over txMetaInputs  (const inputs)
                           . over txMetaOutputs (const outputs)


-- | Handy wrapper to be able to compare things with the 'isomorphicTo'
-- combinator, which ignores the different order of the inputs & outputs.
data DeepEqual = DeepEqual TxMeta

instance Eq DeepEqual where
    (DeepEqual t1) == (DeepEqual t2) = t1 `exactlyEqualTo` t2

instance Buildable DeepEqual where
    build (DeepEqual t) = build t

data Isomorphic = Isomorphic TxMeta

instance Eq Isomorphic where
    (Isomorphic t1) == (Isomorphic t2) = t1 `isomorphicTo` t2

instance Buildable Isomorphic where
    build (Isomorphic t) = build t

instance Buildable [Isomorphic] where
    build ts = bprint (listJsonIndent 4) ts

sortByAmount :: SortDirection -> [Isomorphic] -> [Isomorphic]
sortByAmount direction = sortBy sortFn
    where
        withDir Ascending  = identity
        withDir Descending = flip

        sortFn (Isomorphic a) (Isomorphic b) =
            (withDir direction compare) (a ^. txMetaAmount) (b ^. txMetaAmount)

sortByCreationAt :: SortDirection -> [Isomorphic] -> [Isomorphic]
sortByCreationAt direction = sortBy sortFn
    where
        withDir Ascending  = identity
        withDir Descending = flip

        sortFn (Isomorphic a) (Isomorphic b) =
            (withDir direction compare) (a ^. txMetaCreationAt) (b ^. txMetaCreationAt)

-- | Specs which tests the persistent storage and API provided by 'TxMeta'.
txMetaStorageSpecs :: Spec
txMetaStorageSpecs = do
    describe "TxMeta equality" $ do
        prop "should be reflexive" $ \blueprint -> do
            unSTB blueprint `exactlyEqualTo` unSTB (blueprint :: ShowThroughBuild TxMeta)

        it "should be strict when needed"
            $ forAll genSimilarTxMetas
            $ \(STB t1, STB t2)  -> not (t1 `exactlyEqualTo` t2)

        it "isomorphicTo is more lenient"
            $ forAll genSimilarTxMetas
            $ \(STB t1, STB t2) -> t1 `isomorphicTo` t2

    describe "TxMeta storage" $ do

        it "can store a TxMeta and retrieve it back" $ monadicIO $ do
            testMetaSTB <- pick arbitrary
            run $ withTemporaryDb $ \hdl -> do
                let testMeta = unSTB testMetaSTB
                void $ putTxMeta hdl testMeta
                mbTx <- getTxMeta hdl (testMeta ^. txMetaId)
                fmap DeepEqual mbTx `shouldBe` Just (DeepEqual testMeta)

        it "yields Nothing when calling getTxMeta, if a TxMeta is not there" $ monadicIO $ do
            testMetaSTB <- pick arbitrary
            run $ withTemporaryDb $ \hdl -> do
                let testMeta = unSTB testMetaSTB
                mbTx <- getTxMeta hdl (testMeta ^. txMetaId)
                fmap DeepEqual mbTx `shouldBe` Nothing

        it "inserting the same tx twice is a no-op" $ monadicIO $ do
            testMetaSTB <- pick arbitrary
            run $ withTemporaryDb $ \hdl -> do
                let testMeta = unSTB testMetaSTB

                putTxMeta hdl testMeta `shouldReturn` ()
                putTxMeta hdl testMeta `shouldReturn` ()

        it "inserting two tx with the same tx, but different content is an error" $ monadicIO $ do
            testMetaSTB <- pick arbitrary
            run $ withTemporaryDb $ \hdl -> do
                let meta1 = unSTB testMetaSTB
                let meta2 = set txMetaIsOutgoing (not $ meta1 ^. txMetaIsOutgoing) meta1

                putTxMeta hdl meta1 `shouldReturn` ()
                putTxMeta hdl meta2 `shouldThrow`
                    (\(InvariantViolated (DuplicatedTransactionWithDifferentHash _)) -> True)

        it "inserting multiple txs and later retrieving all of them works" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 10)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                result <- getTxMetas hdl (Offset 0) (Limit 100) Nothing
                map Isomorphic result `shouldMatchList` map Isomorphic metas

        it "pagination correctly limit the results" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 15)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                result <- getTxMetas hdl (Offset 0) (Limit 10) Nothing
                length result `shouldBe` 10

        it "pagination correctly sorts (ascending) the results" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 10)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                result <- (getTxMetas hdl) (Offset 0) (Limit 10) (Just $ Sorting SortByAmount Ascending)
                map Isomorphic result `shouldBe` sortByAmount Ascending (map Isomorphic metas)

        it "pagination correctly sorts (descending) the results" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 10)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                result <- (getTxMetas hdl) (Offset 0) (Limit 10) (Just $ Sorting SortByCreationAt Descending)
                map Isomorphic result `shouldBe` sortByCreationAt Descending (map Isomorphic metas)
