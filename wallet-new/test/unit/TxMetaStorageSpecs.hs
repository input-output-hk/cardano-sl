{-# LANGUAGE RankNTypes #-}
module TxMetaStorageSpecs (txMetaStorageSpecs) where

import           Universum

import           Cardano.Wallet.Kernel.DB.TxMeta
import           Control.Exception.Safe (bracket, catchJust)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text.Buildable (build)

import           Formatting (bprint)
import           Serokell.Util.Text (listJsonIndent)
import           System.Directory (getTemporaryDirectory, removeFile, withCurrentDirectory)
import           System.IO.Error (isDoesNotExistError)
import           Test.Hspec (shouldThrow)
import           Test.QuickCheck (arbitrary, generate, resize, vectorOf)
import           Util.Buildable.Hspec

import qualified Cardano.Wallet.Kernel.DB.Sqlite as Storage

-- | Handy combinator which yields a fresh database to work with on each spec.
withTemporaryDb :: forall m a. (MonadIO m, MonadMask m) => (MetaDBHandle -> m a) -> m a
withTemporaryDb action = bracket acquire release action
    where
       acquire :: m MetaDBHandle
       acquire = liftIO $ do
           tmpDir <- getTemporaryDirectory
           withCurrentDirectory tmpDir $ do
               -- Remove any existing db, if any.
               catchJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                         (removeFile "txmeta_test.db")
                         (\_ -> return ())
               db <- openMetaDB "txmeta_test.db"
               Storage.unsafeMigrate db
               return db

       release :: MetaDBHandle -> m ()
       release = liftIO . closeMetaDB


-- | Generates two 'TxMeta' which are @almost@ identical, if not in the
-- arrangement of their inputs & outputs.
genSimilarTxMetas :: IO (TxMeta, TxMeta)
genSimilarTxMetas = liftIO $ do
    inputs  <- generate (resize 5 arbitrary)
    outputs <- generate (resize 5 arbitrary)
    blueprint <- generate arbitrary
    let t1 = blueprint & over txMetaInputs  (const inputs)
                       . over txMetaOutputs (const outputs)
    let t2 = blueprint & over txMetaInputs  (const (NonEmpty.reverse inputs))
                       . over txMetaOutputs (const (NonEmpty.reverse outputs))
    return (t1, t2)


-- | Handy wrapper to be able to compare things with the 'shallowEq'
-- combinator, which ignores the different order of the inputs & outputs.
data DeepEqual = DeepEqual TxMeta

instance Eq DeepEqual where
    (DeepEqual t1) == (DeepEqual t2) = t1 `deepEq` t2

instance Buildable DeepEqual where
    build (DeepEqual t) = build t

data ShallowEqual = ShallowEqual TxMeta

instance Eq ShallowEqual where
    (ShallowEqual t1) == (ShallowEqual t2) = t1 `shallowEq` t2

instance Buildable ShallowEqual where
    build (ShallowEqual t) = build t

instance Buildable [ShallowEqual] where
    build ts = bprint (listJsonIndent 4) ts

-- | Specs which tests the persistent storage and API provided by 'TxMeta'.
txMetaStorageSpecs :: Spec
txMetaStorageSpecs = do
    describe "TxMeta equality" $ do
        it "should be reflexive" $ do
            (blueprint :: TxMeta) <- liftIO $ generate arbitrary
            blueprint `deepEq` blueprint `shouldBe` True

        it "should be deep" $ do
            (t1, t2) <- liftIO genSimilarTxMetas
            t1 `deepEq` t2 `shouldBe` False

        it "shallowEq is really shallow" $ do
            (t1, t2) <- liftIO genSimilarTxMetas
            t1 `shallowEq` t2 `shouldBe` True

    describe "TxMeta storage" $ do

        it "can store a TxMeta and retrieve it back" $ do
            withTemporaryDb $ \hdl -> do
                testMeta <- liftIO $ generate arbitrary
                void $ putTxMeta hdl testMeta
                mbTx <- getTxMeta hdl (testMeta ^. txMetaId)
                fmap DeepEqual mbTx `shouldBe` Just (DeepEqual testMeta)

        it "yields Nothing when calling getTxMeta, if a TxMeta is not there" $ do
            withTemporaryDb $ \hdl -> do
                testMeta <- liftIO $ generate arbitrary
                mbTx <- getTxMeta hdl (testMeta ^. txMetaId)
                fmap DeepEqual mbTx `shouldBe` Nothing

        it "inserting the same tx twice is a no-op" $ do
            withTemporaryDb $ \hdl -> do
                testMeta <- liftIO $ generate arbitrary

                putTxMeta hdl testMeta `shouldReturn` ()
                putTxMeta hdl testMeta `shouldReturn` ()

        it "inserting two tx with the same tx, but different content is an error" $ do
            withTemporaryDb $ \hdl -> do
                meta1 <- liftIO $ generate arbitrary
                let meta2 = set txMetaIsOutgoing (not $ meta1 ^. txMetaIsOutgoing) meta1

                putTxMeta hdl meta1 `shouldReturn` ()
                putTxMeta hdl meta2 `shouldThrow`
                    (\(InvariantViolated (DuplicatedTransactionWithDifferentHash _)) -> True)

        it "inserting multiple txs and later retrieving all of them works" $ do
            withTemporaryDb $ \hdl -> do
                metas <- liftIO $ generate (vectorOf 1 arbitrary)
                forM_ metas (putTxMeta hdl)
                result <- getTxMetas hdl (Offset 0) (Limit 100) Nothing
                map ShallowEqual result `shouldMatchList` map ShallowEqual metas

        it "pagination correctly limit the results" $ do
            withTemporaryDb $ \hdl -> do
                metas <- liftIO $ generate (vectorOf 100 arbitrary)
                forM_ metas (putTxMeta hdl)
                result <- getTxMetas hdl (Offset 0) (Limit 10) Nothing
                length result `shouldBe` 10
