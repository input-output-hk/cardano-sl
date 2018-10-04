{-# LANGUAGE RankNTypes #-}
module TxMetaStorageSpecs

 (
      txMetaStorageSpecs
    , genMeta
    , Isomorphic (..)
    ) where

import           Universum

import qualified Cardano.Wallet.Kernel.DB.Sqlite as SQlite
import           Cardano.Wallet.Kernel.DB.TxMeta
import           Control.Exception.Safe (bracket)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           Formatting.Buildable (build)

import           Pos.Chain.Txp (TxId)
import qualified Pos.Core as Core

import           Formatting (bprint)
import           Serokell.Util.Text (listJsonIndent, pairF)
import           Test.Hspec (expectationFailure, shouldContain, shouldThrow)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, forAll, suchThat,
                     vectorOf)
import           Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import           Util.Buildable (ShowThroughBuild (..))
import           Util.Buildable.Hspec

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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

-- | Generates two 'TxMeta' which are @almost@ identical (not in the
-- arrangement of their inputs). This means that isomorphicTo should
-- succeed, while exactlyEqualTo not.
genSimilarTxMetas :: Gen (ShowThroughBuild TxMeta, ShowThroughBuild TxMeta)
genSimilarTxMetas = do
    inputs  <- fmap getInput <$> uniqueElements 5
    outputs <- NonEmpty.fromList <$> vectorOf 5 arbitrary
    blueprint <- unSTB <$> genMeta
    let t1 = blueprint & over txMetaInputs  (const inputs)
                       . over txMetaOutputs (const outputs)
    let t2 = blueprint & over txMetaInputs  (const (NonEmpty.reverse inputs))
                       . over txMetaOutputs (const (outputs)) -- order is important for outputs.
    return (STB t1, STB t2)

-- | Synthetic @newtype@ used to generate unique inputs as part of
-- 'genMetas'. The reason why it is necessary is because the stock implementation
-- of 'Eq' would of course declare two tuples equal if their elements are.
-- However, this is too \"strong\" for our 'uniqueElements' generator.
newtype Input = Input { getInput :: (TxId, Word32, Core.Address, Core.Coin) }
    deriving (Show)

-- | Eq is defined on the primary keys of Inputs.
instance Eq Input where
    (Input (id1, ix1, _, _)) == (Input (id2, ix2, _, _)) = (id1, ix1) == (id2, ix2)

-- | This ensures that Inputs generated for the same Tx from getMetas, do not
-- double spend as they don`t use the same output.
instance Ord Input where
    compare (Input (id1, ix1, _, _)) (Input (id2, ix2, _, _)) = compare (id1, ix1) (id2, ix2)

instance Arbitrary Input where
    arbitrary = Input <$> arbitrary

instance Buildable Input where
    build (Input b) = bprint quadF b

instance Buildable (Int, Input) where
    build b = bprint pairF b

instance Buildable [Input] where
    build = bprint (listJsonIndent 4)

newtype Output = Output { getOutput :: (Core.Address, Core.Coin) }

instance Arbitrary Output where
    arbitrary = Output  <$> arbitrary

-- | Handy generator which make sure we are generating 'TxMeta' which all
-- have distinct txids and valid Inputs.
-- This gives the promise that all @size@ number of TxMeta can succesfully be
-- inserted in an empty db, with no violation.
genMetas :: Int -> Gen [ShowThroughBuild TxMeta]
genMetas size = do
    txids <- uniqueElements size
    (metas1 :: [TxMeta])  <- map unSTB <$> vectorOf size genMeta
    let metas = (\(txid, m) -> m{_txMetaId = txid}) <$> zip (NonEmpty.toList txids) metas1
    return $ map STB metas

-- | Generator for an arbitrary 'TxMeta' with valid Inputs.
-- This gives the promise that it can succesfully be inserted in an empty db,
-- with no violation.
genMeta :: Gen (ShowThroughBuild TxMeta)
genMeta = do
    meta <- TxMeta <$> arbitrary
                   <*> arbitrary
                   <*> (fmap getInput  <$> uniqueElements 2)
                   <*> (fmap getOutput . NonEmpty.fromList <$> vectorOf 2 arbitrary)
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
    pure (STB meta)

newtype TxMetaWrapper = TxMetaWrapper (ShowThroughBuild TxMeta) deriving Show

instance Arbitrary TxMetaWrapper where
    arbitrary = TxMetaWrapper <$> genMeta

-- | Handy wrapper to be able to compare things with the 'isomorphicTo'
-- combinator, which ignores the different order of the inputs & outputs.
data DeepEqual = DeepEqual TxMeta

instance Eq DeepEqual where
    (DeepEqual t1) == (DeepEqual t2) = t1 `exactlyEqualTo` t2

instance Buildable DeepEqual where
    build (DeepEqual t) = build t

data Isomorphic = Isomorphic TxMeta deriving Show

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


hasDupes :: Ord a => [a] -> Bool
hasDupes xs = length (Set.fromList xs) /= List.length xs

-- | Specs which tests the persistent storage and API provided by 'TxMeta'.
txMetaStorageSpecs :: Spec
txMetaStorageSpecs = do
    describe "migrations" $ do
        it "calling migration second time does nothing" $ monadicIO $ do
            testMetaSTB <- pick genMeta
            let testMeta = unSTB testMetaSTB
            db <- liftIO $ openMetaDB ":memory:"
            liftIO $ migrateMetaDB db
            liftIO $ putTxMeta db testMeta
            mbTx1 <- liftIO $ getTxMeta db (testMeta ^. txMetaId) (testMeta ^. txMetaWalletId) (testMeta ^. txMetaAccountIx)
            liftIO $ migrateMetaDB db
            mbTx2 <- liftIO $ getTxMeta db (testMeta ^. txMetaId) (testMeta ^. txMetaWalletId) (testMeta ^. txMetaAccountIx)
            liftIO $ Isomorphic <$> mbTx1 `shouldBe` Just (Isomorphic testMeta)
            liftIO $ Isomorphic <$> mbTx2 `shouldBe` Just (Isomorphic testMeta)
            liftIO $ closeMetaDB db

        it "calling clearMetaDB wipes entries" $ monadicIO $ do
            testMetaSTB <- pick genMeta
            let testMeta = unSTB testMetaSTB
            db <- liftIO $ openMetaDB ":memory:"
            liftIO $ migrateMetaDB db
            liftIO $ putTxMeta db testMeta
            mbTx1 <- liftIO $ getTxMeta db (testMeta ^. txMetaId) (testMeta ^. txMetaWalletId) (testMeta ^. txMetaAccountIx)
            liftIO $ clearMetaDB db
            mbTx2 <- liftIO $ getTxMeta db (testMeta ^. txMetaId) (testMeta ^. txMetaWalletId) (testMeta ^. txMetaAccountIx)
            liftIO $ Isomorphic <$> mbTx1 `shouldBe` Just (Isomorphic testMeta)
            liftIO $ Isomorphic <$> mbTx2 `shouldBe` Nothing
            liftIO $ closeMetaDB db

    describe "uniqueElements generator" $ do
        it "generates unique inputs" $ monadicIO $ do
            (inputs :: NonEmpty (ShowThroughBuild Input)) <- pick (uniqueElements 30)
            assert (not $ hasDupes . map unSTB . toList $ inputs)

    describe "TxMeta equality" $ do
        prop "should be reflexive" $ \(TxMetaWrapper blueprint) -> do
            unSTB blueprint `exactlyEqualTo` unSTB (blueprint :: ShowThroughBuild TxMeta)

        it "should be strict when needed"
            $ forAll genSimilarTxMetas
            $ \(STB t1, STB t2)  -> not (t1 `exactlyEqualTo` t2)

        it "isomorphicTo is more lenient"
            $ forAll genSimilarTxMetas
            $ \(STB t1, STB t2) -> t1 `isomorphicTo` t2

        it "fromInputs . mkInputs should keep same inputs" $ monadicIO $ do
            meta <- unSTB <$> pick genMeta
            let ls = SQlite.fromInputs . SQlite.mkInputs $ meta
            return $ Isomorphic meta{_txMetaInputs = ls} `shouldBe` Isomorphic meta

        it "fromOutputs . mkOutputs should keep the same ordered outputs" $ monadicIO $ do
            meta <- unSTB <$> pick genMeta
            let ls = SQlite.fromOutputs . SQlite.mkOutputs $ meta
            return $ meta{_txMetaOutputs = ls} `exactlyEqualTo` meta

    describe "TxMeta storage" $ do

        it "can store a TxMeta and retrieve it back" $ monadicIO $ do
            testMetaSTB <- pick genMeta
            run $ withTemporaryDb $ \hdl -> do
                let testMeta = unSTB testMetaSTB
                void $ putTxMeta hdl testMeta
                mbTx <- getTxMeta hdl (testMeta ^. txMetaId) (testMeta ^. txMetaWalletId) (testMeta ^. txMetaAccountIx)
                Isomorphic <$> mbTx `shouldBe` Just (Isomorphic testMeta)

        it "yields Nothing when calling getTxMeta, if a TxMeta is not there" $ monadicIO $ do
            testMetaSTB <- pick genMeta
            run $ withTemporaryDb $ \hdl -> do
                let testMeta = unSTB testMetaSTB
                mbTx <- getTxMeta hdl (testMeta ^. txMetaId) (testMeta ^. txMetaWalletId) (testMeta ^. txMetaAccountIx)
                fmap DeepEqual mbTx `shouldBe` Nothing

        it "inserting the same tx twice is a no-op" $ monadicIO $ do
            testMetaSTB <- pick genMeta
            run $ withTemporaryDb $ \hdl -> do
                let testMeta = unSTB testMetaSTB

                putTxMetaT hdl testMeta `shouldReturn` Tx
                putTxMetaT hdl testMeta `shouldReturn` No

        it "inserting two tx with the same PrimaryKey, but different content is an no-op" $ monadicIO $ do
            -- Double insertion may happen in rollback.
            -- The meta may be different e.g. different timestamps.
            -- So no errors here.
            testMetaSTB <- pick genMeta
            run $ withTemporaryDb $ \hdl -> do
                let meta1 = unSTB testMetaSTB
                let meta2 = set txMetaIsOutgoing (not $ meta1 ^. txMetaIsOutgoing) meta1

                putTxMetaT hdl meta1 `shouldReturn` Tx
                putTxMetaT hdl meta2 `shouldReturn` No

        it "same account can`t insert the same tx with different Inputs" $ monadicIO $ do
            ins <- map getInput  <$> pick (uniqueElements 4)
            let in1 : in2 : in3 : in4 : _ = NonEmpty.toList ins
            let inp1 = NonEmpty.fromList [in1,in2]
            let inp2 = NonEmpty.fromList [in3,in4]
            meta <- unSTB <$> pick genMeta
            run $ withTemporaryDb $ \hdl -> do
                let meta1 = meta {_txMetaInputs = inp1}
                let meta2 = meta {_txMetaInputs = inp2}
                putTxMetaT hdl meta1 `shouldReturn` Tx
                putTxMetaT hdl meta2 `shouldThrow`
                    (\(InvariantViolated (TxIdInvariantViolated _)) -> True)

        it "two accounts can`t insert the same tx with different Inputs" $ monadicIO $ do
            ins <- map getInput  <$> pick (uniqueElements 4)
            let in1 : in2 : in3 : in4 : _ = NonEmpty.toList ins
            let inp1 = NonEmpty.fromList [in1,in2]
            let inp2 = NonEmpty.fromList [in3,in4]
            meta <- unSTB <$> pick genMeta
            let accountIx1 = meta ^. txMetaAccountIx
            accountIx2 <- pick $ suchThat arbitrary (\ix -> ix /= accountIx1)
            run $ withTemporaryDb $ \hdl -> do
                let walletId   = meta ^. txMetaWalletId
                let meta1 = meta {_txMetaInputs = inp1}
                let meta2 = meta {_txMetaInputs = inp2, _txMetaAccountIx = accountIx2}
                let txId = (meta ^. txMetaId)
                putTxMeta hdl meta1
                putTxMeta hdl meta2 `shouldThrow`
                    (\(InvariantViolated (TxIdInvariantViolated _)) -> True)
                txIdIsomorphic meta1 meta2 `shouldBe` False
                metaRes1 <- getTxMeta hdl txId walletId accountIx1
                metaRes2 <- getTxMeta hdl txId walletId accountIx2
                case (metaRes1, metaRes2) of
                    (Just m1, Nothing) -> do
                        Isomorphic m1 `shouldBe` Isomorphic meta1
                        ([m], count) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything Nothing (FilterByIndex txId) NoFilterOp Nothing
                        Isomorphic m `shouldBe` Isomorphic m1
                        count `shouldBe` (Just 1)
                    (_, _) -> expectationFailure "only the first get should succeed"


        it "two accounts can`t insert the same tx with different Outputs" $ monadicIO $ do
            meta <- unSTB <$> pick genMeta
            let out1 = _txMetaOutputs meta
            let out2 = NonEmpty.reverse out1
            let accountIx1 = meta ^. txMetaAccountIx
            accountIx2 <- pick $ suchThat arbitrary (\ix -> ix /= accountIx1)
            run $ withTemporaryDb $ \hdl -> do
                let walletId   = meta ^. txMetaWalletId
                let meta1 = meta {_txMetaOutputs = out1}
                let meta2 = meta {_txMetaOutputs = out2, _txMetaAccountIx = accountIx2}
                let txId = (meta ^. txMetaId)
                putTxMeta hdl meta1
                putTxMeta hdl meta2 `shouldThrow`
                    (\(InvariantViolated (TxIdInvariantViolated _)) -> True)
                txIdIsomorphic meta1 meta2 `shouldBe` False
                metaRes1 <- getTxMeta hdl txId walletId accountIx1
                metaRes2 <- getTxMeta hdl txId walletId accountIx2
                case (metaRes1, metaRes2) of
                    (Just m1, Nothing) -> do
                        Isomorphic m1 `shouldBe` Isomorphic meta1
                        ([m], count) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything Nothing (FilterByIndex txId) NoFilterOp Nothing
                        Isomorphic m `shouldBe` Isomorphic m1
                        count `shouldBe` (Just 1)
                    (_, _) -> expectationFailure "only the first get should succeed"

        it "two accounts can succesfully insert the same tx" $ monadicIO $ do
            meta1 <- unSTB <$> pick genMeta
            let accountIx1 = meta1 ^. txMetaAccountIx
            accountIx2 <- pick $ suchThat arbitrary (\ix -> ix /= accountIx1)
            run $ withTemporaryDb $ \hdl -> do
                let walletId   = meta1 ^. txMetaWalletId
                let meta2 = meta1 {_txMetaAccountIx = accountIx2}
                let txId = (meta1 ^. txMetaId)
                putTxMeta hdl meta1
                putTxMeta hdl meta2
                txIdIsomorphic meta1 meta2 `shouldBe` True
                metaRes1 <- getTxMeta hdl txId walletId accountIx1
                metaRes2 <- getTxMeta hdl txId walletId accountIx2
                case (metaRes1, metaRes2) of
                    (Just m1, Just m2) -> do
                        Isomorphic m1 `shouldBe` Isomorphic meta1
                        Isomorphic m2 `shouldBe` Isomorphic meta2
                        (result, count) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything Nothing (FilterByIndex txId) NoFilterOp Nothing
                        map Isomorphic result `shouldContain` [Isomorphic m1]
                        map Isomorphic result `shouldContain` [Isomorphic m2]
                        count `shouldBe` (Just 2)
                    (_, _) -> expectationFailure "can`t retrieve same tx"

        it "two accounts can succesfully insert the same tx, one Incoming the other Outgoing" $ monadicIO $ do
            meta <- unSTB <$> pick genMeta
            let accountIx1 = meta ^. txMetaAccountIx
            accountIx2 <- pick $ suchThat arbitrary (\ix -> ix /= accountIx1)
            run $ withTemporaryDb $ \hdl -> do
                let walletId   = meta ^. txMetaWalletId
                let meta1 = meta  {_txMetaIsOutgoing = True}
                let meta2 = meta1 {_txMetaAccountIx = accountIx2, _txMetaIsOutgoing = False}
                let txId = (meta1 ^. txMetaId)
                putTxMeta hdl meta1
                putTxMeta hdl meta2
                txIdIsomorphic meta1 meta2 `shouldBe` True
                metaRes1 <- getTxMeta hdl txId walletId accountIx1
                metaRes2 <- getTxMeta hdl txId walletId accountIx2
                case (metaRes1, metaRes2) of
                    (Just m1, Just m2) -> do
                        Isomorphic m1 `shouldBe` Isomorphic meta1
                        Isomorphic m2 `shouldBe` Isomorphic meta2
                        (result, count) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything Nothing (FilterByIndex txId) NoFilterOp Nothing
                        map Isomorphic result `shouldContain` [Isomorphic m1]
                        map Isomorphic result `shouldContain` [Isomorphic m2]
                        count `shouldBe` (Just 2)
                    (_, _) -> expectationFailure "can`t retrieve same tx"

        it "one account can succesfully insert two tx" $ monadicIO $ do
            meta1 <- unSTB <$> pick genMeta
            let txId1 = meta1 ^. txMetaId
            txId2 <- pick $ suchThat arbitrary (\ix -> ix /= txId1)
            run $ withTemporaryDb $ \hdl -> do
                let walletId   = meta1 ^. txMetaWalletId
                let accountIx = meta1 ^. txMetaAccountIx
                let meta2 = meta1 {_txMetaId = txId2}
                putTxMeta hdl meta1
                putTxMeta hdl meta2
                metaRes1 <- getTxMeta hdl txId1 walletId accountIx
                metaRes2 <- getTxMeta hdl txId2 walletId accountIx
                case (metaRes1, metaRes2) of
                    (Just m1, Just m2) -> do
                        Isomorphic m1 `shouldBe` Isomorphic meta1
                        Isomorphic m2 `shouldBe` Isomorphic meta2
                        (result, count) <- (getTxMetas hdl) (Offset 0) (Limit 5) (AccountFops walletId (Just accountIx)) Nothing NoFilterOp NoFilterOp Nothing
                        map Isomorphic result `shouldContain` [Isomorphic m1]
                        map Isomorphic result `shouldContain` [Isomorphic m2]
                        count `shouldBe` (Just 2)
                    (_, _) -> expectationFailure "can`t retrieve both txs"

        it "inserting multiple txs and later retrieving all of them works" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                (result, _) <- getTxMetas hdl (Offset 0) (Limit 100) Everything Nothing NoFilterOp NoFilterOp Nothing
                map Isomorphic result `shouldMatchList` map Isomorphic metas

        it "pagination correctly limit the results" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 10)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                (result, _) <- getTxMetas hdl (Offset 0) (Limit 5) Everything Nothing NoFilterOp NoFilterOp Nothing
                length result `shouldBe` 5

        it "pagination correctly sorts (ascending) the results" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                (result, _) <- (getTxMetas hdl) (Offset 0) (Limit 10) Everything Nothing NoFilterOp NoFilterOp (Just $ Sorting SortByAmount Ascending)
                map Isomorphic result `shouldBe` sortByAmount Ascending (map Isomorphic metas)

        it "pagination correctly sorts (descending) the results" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                (result, _) <- (getTxMetas hdl) (Offset 0) (Limit 10) Everything Nothing NoFilterOp NoFilterOp (Just $ Sorting SortByCreationAt Descending)
                map Isomorphic result `shouldBe` sortByCreationAt Descending (map Isomorphic metas)

        it "implicit sorting is by creation time descenting" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                (result, _) <- (getTxMetas hdl) (Offset 0) (Limit 10) Everything Nothing NoFilterOp NoFilterOp Nothing
                map Isomorphic result `shouldBe` sortByCreationAt Descending (map Isomorphic metas)

        it "metadb counts total Entries properly" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 10)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                forM_ metas (putTxMeta hdl)
                (_, total) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything Nothing NoFilterOp NoFilterOp Nothing
                total `shouldBe` (Just 10)

        it "filtering walletid works ok" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 10)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case walletIdTransform metas of
                    Nothing -> error "txMeta was found with less elements than it should"
                    Just (metasW, accFop, expectedResults) -> do
                        forM_ metasW (putTxMeta hdl)
                        (result, total) <- (getTxMetas hdl) (Offset 0) (Limit 20) accFop Nothing NoFilterOp NoFilterOp Nothing
                        map Isomorphic result `shouldMatchList` map Isomorphic expectedResults
                        total `shouldBe` (Just $ length expectedResults)

        it "multiple filters works ok" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 10)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case filtersTransform metas of
                    Nothing -> error "txMeta was found with less elements than it should"
                    Just (metasF, accFop, fopTimestamp, expectedResults) -> do
                        forM_ metasF (putTxMeta hdl)
                        (result, total) <- (getTxMetas hdl) (Offset 0) (Limit 10) accFop Nothing NoFilterOp fopTimestamp Nothing
                        map Isomorphic result `shouldMatchList` map Isomorphic expectedResults
                        total `shouldBe` (Just $ length expectedResults)

        it "pagination and filtering" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case filtersTransform metas of
                    Nothing -> expectationFailure "txMeta was found with less elements than it should"
                    Just (metasF, accFop, fopTimestamp, expectedResults) -> do
                        forM_ metasF (putTxMeta hdl)
                        (result, total) <- (getTxMetas hdl) (Offset 0) (Limit 1) accFop Nothing NoFilterOp fopTimestamp Nothing
                        map Isomorphic expectedResults `shouldContain` map Isomorphic result
                        total `shouldBe` (Just $ length expectedResults)

        it "fitlering addresses" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddress metas of
                    Nothing -> error "txMeta was found with less elements than it should"
                    Just (addr, m) -> do
                        forM_ metas (putTxMeta hdl)
                        (result, _) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything (Just addr) NoFilterOp NoFilterOp Nothing
                        map Isomorphic result `shouldContain` [Isomorphic m]

        it "returns meta with the correct address in Inputs or Outputs (SQL union)" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddressTransform metas of
                    Nothing -> error "txMeta was found with less elements than it should"
                    Just (metasA, addr, m1, m2) -> do
                        forM_ metasA (putTxMeta hdl)
                        (result, count) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything (Just addr) NoFilterOp NoFilterOp Nothing
                        let iso = map Isomorphic result
                        count `shouldSatisfy` (justbeq 2)
                        iso  `shouldContain` [Isomorphic m1]
                        iso  `shouldContain` [Isomorphic m2]

        it "txs with same address in both inputs and outputs are reported once (SQL union removes duplicates)" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddressTransform' metas of
                    Nothing -> error "txMeta was found with less elements than it should"
                    Just (metasA, addr, m1, m2) -> do
                        -- m2 has addr in both inputs and outputs.
                        forM_ metasA (putTxMeta hdl)
                        (result, count) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything (Just addr) NoFilterOp NoFilterOp Nothing
                        let iso = map Isomorphic result
                        count `shouldSatisfy` (justbeq 2)
                        iso  `shouldContain` [Isomorphic m1]
                        iso  `shouldContain` [Isomorphic m2]
                        let sameTxIdWithM2 = filter (\m -> _txMetaId m == _txMetaId m2) result
                        map Isomorphic sameTxIdWithM2 `shouldBe` [Isomorphic m2]


        it "paginates meta with the correct address in Inputs or Outputs" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddressTransform metas of
                    Nothing -> expectationFailure "txMeta was found with less elements than it should"
                    Just (metasA, addr, m1, m2) -> do
                        forM_ metasA (putTxMeta hdl)
                        (result1, count1) <- (getTxMetas hdl) (Offset 0) (Limit 1) Everything (Just addr) NoFilterOp NoFilterOp Nothing
                        (result2, count2) <- (getTxMetas hdl) (Offset 1) (Limit 4) Everything (Just addr) NoFilterOp NoFilterOp Nothing
                        let result = result1 <> result2
                        let iso = map Isomorphic result
                        length result1 `shouldBe` 1
                        length result2 `shouldSatisfy` (>= 1)
                        count1 `shouldSatisfy` (justbeq 2)
                        count2 `shouldSatisfy` (justbeq 2)
                        count1 `shouldBe` count2
                        iso  `shouldContain` [Isomorphic m1]
                        iso  `shouldContain` [Isomorphic m2]

        it "filters on txid meta with the correct address in Inputs or Outputs" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddressTransform metas of
                    Nothing -> expectationFailure "txMeta was found with less elements than it should"
                    Just (metasA, addr, m1, _) -> do
                        let txid = _txMetaId m1
                        forM_ metasA (putTxMeta hdl)
                        (result, count) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything (Just addr) (FilterByIndex txid) NoFilterOp Nothing
                        count `shouldBe` (Just 1) -- here it`s exactly one because we filter on TxId.
                        map Isomorphic result `shouldBe` [Isomorphic m1]

        it "correctly filters on txid meta with the correct address in Inputs or Outputs" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddressTransform metas of
                    Nothing -> expectationFailure "txMeta was found with less elements than it should"
                    Just (metasA, addr, m1, _) -> do
                        let txid = _txMetaId m1
                        forM_ metasA (putTxMeta hdl)
                        (result, count) <- (getTxMetas hdl) (Offset 0) (Limit 5) Everything (Just addr) (FilterByIndex txid) NoFilterOp Nothing
                        count `shouldBe` (Just 1)
                        map Isomorphic result `shouldBe` [Isomorphic m1]


        it "pagination sorts meta with the correct address in Inputs or Outputs (SQL: union, sorting, pagination)" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddressTransform metas of
                    Nothing -> expectationFailure "txMeta was found with less elements than it should"
                    Just (metasA, addr, m1, m2) -> do
                        forM_ metasA (putTxMeta hdl)
                        (result1, count1) <- (getTxMetas hdl) (Offset 0) (Limit 1) Everything (Just addr) NoFilterOp NoFilterOp (Just $ Sorting SortByCreationAt Descending)
                        (result2, count2) <- (getTxMetas hdl) (Offset 1) (Limit 4) Everything (Just addr) NoFilterOp NoFilterOp (Just $ Sorting SortByCreationAt Descending)
                        let result = filter (\m -> _txMetaId m `elem` map _txMetaId [m1, m2] ) (result1 <> result2)
                        length result1 `shouldBe` 1
                        length result `shouldBe` 2
                        -- it`s possible that the generator created the
                        -- same address more than once. So we may expect more than 2 results here.
                        count1 `shouldSatisfy` (justbeq 2)
                        count2 `shouldSatisfy` (justbeq 2)
                        count1 `shouldBe` count2
                        map Isomorphic result `shouldBe` sortByCreationAt Descending (map Isomorphic [m1, m2])

        it "like above, but we test implicit sorting" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddressTransform metas of
                    Nothing -> expectationFailure "txMeta was found with less elements than it should"
                    Just (metasA, addr, m1, m2) -> do
                        forM_ metasA (putTxMeta hdl)
                        (result1, count1) <- (getTxMetas hdl) (Offset 0) (Limit 1) Everything (Just addr) NoFilterOp NoFilterOp Nothing
                        (result2, count2) <- (getTxMetas hdl) (Offset 1) (Limit 4) Everything (Just addr) NoFilterOp NoFilterOp Nothing
                        let result = filter (\m -> _txMetaId m `elem` map _txMetaId [m1, m2] ) (result1 <> result2)
                        length result1 `shouldBe` 1
                        length result `shouldBe` 2
                        -- it`s possible that the generator created the
                        -- same address more than once. So we may expect more than 2 results here.
                        count1 `shouldSatisfy` (justbeq 2)
                        count2 `shouldSatisfy` (justbeq 2)
                        count1 `shouldBe` count2
                        map Isomorphic result `shouldBe` sortByCreationAt Descending (map Isomorphic [m1, m2])

        it "applying all filters succeeds when it should" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddress metas of
                    Nothing -> expectationFailure "txMeta was found with less elements than it should"
                    Just (addr,m@ TxMeta{..}) -> do
                        forM_ metas (putTxMeta hdl)
                        (result, total) <- (getTxMetas hdl)
                                (Offset 0)
                                (Limit 5)
                                (AccountFops _txMetaWalletId $ Just _txMetaAccountIx)
                                (Just addr)
                                (FilterByPredicate Equal _txMetaId)
                                (FilterByPredicate GreaterThanEqual _txMetaCreationAt)
                                (Just $ Sorting SortByCreationAt Descending)
                        map Isomorphic result `shouldMatchList` [Isomorphic m]
                        total `shouldBe` (Just 1)

        it "applying all filters rejects everything when it should" $ monadicIO $ do
            testMetasSTB <- pick (genMetas 5)
            run $ withTemporaryDb $ \hdl -> do
                let metas = map unSTB testMetasSTB
                case getAddress metas of
                    Nothing -> expectationFailure "txMeta was found with less elements than it should"
                    Just (addr, TxMeta{..}) -> do
                        forM_ metas (putTxMeta hdl)
                        (result, total) <- (getTxMetas hdl)
                                (Offset 0)
                                (Limit 5)
                                (AccountFops _txMetaWalletId $ Just _txMetaAccountIx)
                                (Just addr)
                                (FilterByPredicate Equal _txMetaId)
                                (FilterByPredicate GreaterThan _txMetaCreationAt)
                                (Just $ Sorting SortByCreationAt Descending)
                        map Isomorphic result `shouldBe` []
                        total `shouldBe` (Just 0)

-- Tests that the values is there and is Bigger or Equal to n.
justbeq :: Int -> Maybe Int -> Bool
justbeq n mb =
    case mb of
        Nothing -> False
        Just x  -> x >= n

-- The following functions transform the TxMeta created by genMeta. The result follows the pattern:
--   Nothing if the inputs [TxMeta] are less than needed
--   Just ([TxMeta_to_be_inserted],  ... filters for getTxMetas ... , [TxMeta_expected_as_result])
walletIdTransform :: [TxMeta] -> Maybe ([TxMeta], AccountFops, [TxMeta])
walletIdTransform ls = case ls of
    [] -> Nothing
    [_] -> Nothing
    a : b : rest ->
        let wid = _txMetaWalletId a
            fn x = x {_txMetaWalletId = wid}
            prd m = _txMetaWalletId m == wid
            c = fn b
        in Just  (a : c : rest,
                 AccountFops (_txMetaWalletId a) Nothing,
                 a : c :  filter prd rest)

filtersTransform :: [TxMeta] -> Maybe ([TxMeta], AccountFops, FilterOperation Core.Timestamp ,[TxMeta])
filtersTransform ls = case ls of
    [] -> Nothing
    [_] -> Nothing
    a : b : rest ->
        let wid = _txMetaWalletId a
            accid = _txMetaAccountIx a
            date = _txMetaCreationAt a
            fn x = x {_txMetaWalletId = wid, _txMetaAccountIx = accid, _txMetaCreationAt = date}
            prd m = _txMetaWalletId m == wid && _txMetaAccountIx m == accid && _txMetaCreationAt m == date
            c = fn b
        in Just (a : c : rest,
                AccountFops wid (Just accid),
                FilterByIndex date,
                a : c :  filter prd rest)

getAddress :: [TxMeta] -> Maybe (Core.Address, TxMeta)
getAddress ls = case ls of
    [] -> Nothing
    m : _ ->
        Just (addr, m)
          where
            (_, _, addr, _) = head $ _txMetaInputs m


-- The address returned is found in the Inputs of the first TxMeta
-- and the Outputs of the second.
getAddressTransform :: [TxMeta] -> Maybe ([TxMeta], Core.Address, TxMeta, TxMeta)
getAddressTransform ls = case ls of
    []  -> Nothing
    [_] -> Nothing
    m1 : m2 : rest ->

        Just (rest <> [m2', m1], addr, m1, m2')
            where
                (_, _, addr, coin) = head $ _txMetaInputs m1
                m2' = m2 {_txMetaOutputs = NonEmpty.fromList [(addr, coin)]}

-- The address returned is found in the Inputs of the first TxMeta
-- and the Outputs of the second.
getAddressTransform' :: [TxMeta] -> Maybe ([TxMeta], Core.Address, TxMeta, TxMeta)
getAddressTransform' ls = case ls of
    []  -> Nothing
    [_] -> Nothing
    m1 : m2 : rest ->

        Just (rest <> [m2', m1], addr, m1, m2')
            where
                (_, _, addr, coin) = head $ _txMetaInputs m1
                m2' = m2 {_txMetaOutputs = NonEmpty.fromList [(addr, coin)], _txMetaInputs = _txMetaInputs m1}
