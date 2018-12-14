{-# LANGUAGE RankNTypes #-}

module TxMetaStorage
    ( spec
    ) where

import           Universum

import           Control.Concurrent.Async
import           Control.Exception.Safe (bracket)
import qualified Data.List.NonEmpty as NonEmpty
import           Formatting (bprint)
import           Formatting.Buildable (build)
import           Serokell.Util.Text (listJsonIndent, pairF)
import           Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, vectorOf,
                     withMaxSuccess)
import           Test.QuickCheck.Monadic (monadicIO, pick, run)

import           Cardano.Wallet.Kernel.DB.TxMeta
import           Pos.Chain.Txp (TxId)
import qualified Pos.Core as Core


{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

spec :: Spec
spec = do
    describe "synchronization" $ do
        it "synchronized with 2 write workers and no-ops" $ withMaxSuccess 5 $  monadicIO $ do
            -- beware of the big data.
            metas <- pick (genMetas 2000)
            let (meta0, meta1) = splitAt (div 2000 2) metas
            run $ withTemporaryDb $ \hdl -> do
                t0 <- async $ threadWriteWithNoOp meta0 hdl
                t1 <- async $ threadWriteWithNoOp meta1 hdl
                traverse_ wait [t0, t1]

        it "synchronized with 2 write workers and no-ops: correct count" $ withMaxSuccess 5 $  monadicIO $ do
            -- beware of the big data.
            metas <- pick (genMetas 200)
            let (meta0, meta1) = splitAt (div 200 2) metas
            run $ withTemporaryDb $ \hdl -> do
                t0 <- async $ threadWriteWithNoOp meta0 hdl
                t1 <- async $ threadWriteWithNoOp meta1 hdl
                traverse_ wait [t0, t1]
                (ls, _count) <- getTxMetas hdl (Offset 0) (Limit 300) Everything Nothing NoFilterOp NoFilterOp Nothing
                length ls `shouldBe` 200

        it "synchronized with 2 write workers" $ withMaxSuccess 5 $  monadicIO $ do
            -- beware of the big data.
            metas <- pick (genMetas 2000)
            let (meta0, meta1) = splitAt (div 2000 2) metas
            run $ withTemporaryDb $ \hdl -> do
                t0 <- async $ threadWrite meta0 hdl
                t1 <- async $ threadWrite meta1 hdl
                traverse_ wait [t0, t1]

        it "synchronized with 2 write workers: correct count" $ withMaxSuccess 5 $  monadicIO $ do
            -- beware of the big data.
            metas <- pick (genMetas 200)
            let (meta0, meta1) = splitAt (div 200 2) metas
            run $ withTemporaryDb $ \hdl -> do
                t0 <- async $ threadWrite meta0 hdl
                t1 <- async $ threadWrite meta1 hdl
                traverse_ wait [t0, t1]
                (ls, _count) <- getTxMetas hdl (Offset 0) (Limit 300) Everything Nothing NoFilterOp NoFilterOp Nothing
                length ls `shouldBe` 200

        it "synchronized 1 write and 1 read workers" $ withMaxSuccess 5 $  monadicIO $ do
            -- beware of the big data.
            metas <- pick (genMetas 2000)
            run $ withTemporaryDb $ \hdl -> do
                t0 <- async $ threadWriteWithNoOp metas hdl
                t1 <- async $ threadRead 2000 hdl
                traverse_ wait [t0, t1]

        it "synchronized 1 write and 1 read workers: correct count" $ withMaxSuccess 5 $  monadicIO $ do
            -- beware of the big data.
            metas <- pick (genMetas 200)
            run $ withTemporaryDb $ \hdl -> do
                t0 <- async $ threadWriteWithNoOp metas hdl
                t1 <- async $ threadRead 200 hdl
                traverse_ wait [t0, t1]
                (ls, _count) <- getTxMetas hdl (Offset 0) (Limit 300) Everything Nothing NoFilterOp NoFilterOp Nothing
                length ls `shouldBe` 200


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
genMetas :: Int -> Gen [TxMeta]
genMetas size = do
    txids <- uniqueElements size
    (metas1 :: [TxMeta])  <- vectorOf size genMeta
    let metas = (\(txid, m) -> m{_txMetaId = txid}) <$> zip (NonEmpty.toList txids) metas1
    return metas

-- | Generator for an arbitrary 'TxMeta' with valid Inputs.
-- This gives the promise that it can succesfully be inserted in an empty db,
-- with no violation.
genMeta :: Gen (TxMeta)
genMeta = TxMeta
    <$> arbitrary
    <*> arbitrary
    <*> (fmap getInput  <$> uniqueElements 2)
    <*> (fmap getOutput . NonEmpty.fromList <$> vectorOf 2 arbitrary)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

threadRead :: Int -> MetaDBHandle -> IO ()
threadRead times hdl = do
    let getNoFilters = getTxMetas hdl (Offset 0) (Limit 100) Everything Nothing NoFilterOp NoFilterOp Nothing
    replicateM_ times getNoFilters

threadWrite :: [TxMeta] -> MetaDBHandle -> IO ()
threadWrite metas hdl = do
    let f meta = do
            putTxMetaT hdl meta `shouldReturn` Tx
    mapM_ f metas

-- here we try to add the same tx 2 times. The second must fail, but without crashing
-- anything, as this is a no-op.
threadWriteWithNoOp :: [TxMeta] -> MetaDBHandle -> IO ()
threadWriteWithNoOp metas hdl = do
    let f meta = do
            putTxMetaT hdl meta `shouldReturn` Tx
            putTxMetaT hdl meta `shouldReturn` No
    mapM_ f metas
